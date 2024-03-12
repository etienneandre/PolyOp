(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * École Centrale Nantes, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Author:        Étienne André
 * Created:       2011/04/27
 * Last modified: 2023/12/01
 *
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
************************************************************)


(**************************************************)
(* Modules *)
(**************************************************)
open Global
open AbstractInput
open Arg
open InputPrinter



let input_error_string = "error in the input file"

(**************************************************)
(* Parsing function *)
(**************************************************)

(* Parse a file and return the abstract structure *)
let parser_lexer the_parser the_lexer file =
	(* Open file *)
	let in_channel = try (open_in file) with
		| Sys_error e -> print_error ("The file " ^ file ^ " could not be opened.\n" ^ e); abort_program (); exit(0)
	in
	
	(* Lexing *)
	let lexbuf = try (Lexing.from_channel in_channel) with
		| Failure f -> print_error ("Lexing error in file " ^ file ^ ": " ^ f); abort_program (); exit(0)
	in

	(* Parsing *)
	let parsing_structure = try(
		the_parser the_lexer lexbuf
	) with
		| ParsingError (symbol_start, symbol_end) ->
			(* Convert the file into a string *)
			let extlib_input = IO.input_channel (open_in file) in
			let file_string = IO.read_all extlib_input in
			(* Create the error message *)
			let error_message =
				if symbol_start >= 0 && symbol_end >= symbol_start then (
					(* Get the symbol *)
					let error_symbol = (String.sub file_string symbol_start (symbol_end - symbol_start)) in
					(* Resize it if too big *)
					let error_symbol =
						if (String.length error_symbol > 15) then
							"..." ^ (String.sub error_symbol (String.length error_symbol - 15) 15)
						else error_symbol
					in
					(* Get the line *)
					let beginning_of_the_file = String.sub file_string 0 symbol_end in
					let lines = Str.split (Str.regexp "\n") beginning_of_the_file in
					let line = List.length lines in
					(* Make the message *)
					"next to '" ^ error_symbol ^ "' at line " ^ (string_of_int line) ^ ".")
				else "somewhere in the file, most probably in the very beginning."
			in
			(* Print the error message *)
			print_error ("Parsing error in file '" ^ file ^ "' " ^ error_message); abort_program (); exit(0)
		| Failure f -> print_error ("Parsing error in file '" ^ file ^ "': " ^ f); abort_program (); exit(0)
	in
	parsing_structure


(**************************************************)
(* System functions *)
(**************************************************)

let write_to_file file_name file_content =
	let oc = open_out file_name in
	(* Write file *)
	output_string oc file_content;
	(* Close channel *)
	close_out oc;
	()



(**************************************************)
(**************************************************)
(* STARTING PROGRAM *)
(**************************************************)
(**************************************************)
;;


(**************************************************)
(* Get the arguments *)
(**************************************************)
let usage_msg = "Usage: PolyOp input_file [options]" in

(* Input file *)
let file = ref "" in

let nb_args = ref 0 in
			
(* Get the verbose mode *)
let rec set_verbose_mode_ref verbose_mode =
		let mode = try verbose_mode_of_string verbose_mode
			with Not_found ->
			print_error ("The verbose mode '" ^ verbose_mode ^ "' is not valid.");
			Arg.usage speclist usage_msg;
			abort_program ();
			exit(0); in
		set_verbose_mode mode

(* Options *)
and speclist = [
	("-verbose", String set_verbose_mode_ref, " Print more or less information. Can be set to 'nodebug', 'standard', 'low', 'medium', 'high', 'total'. Default: 'nodebug'");
	("-version", Unit (fun _ -> print_string (version_string ()); exit 0), " Print version string and exit.");

] in
		
(* function for parsing arguments *)
let anon_fun = (fun arg ->
	(* If 1st argument: main file *)
	if !nb_args = 0 then(
		nb_args := !nb_args + 1;
		file := arg;
	)
	(* If more than one argument : warns *)
	else (
		print_warning ("The input argument '" ^ arg ^ "' will be ignored.");
	)
) in

Arg.parse speclist anon_fun usage_msg;

(* Case no file *)
if !nb_args < 1 then(
	 print_error ("Please give a source file name."); 
	Arg.usage speclist usage_msg;
	abort_program (); exit(0)
(* 	file := "test.polyop" *)
);

(* Definition of output *)
let output_file_name = !file ^ ".res" in

(* If the input does not succeed: *)
write_to_file output_file_name input_error_string;


(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)

Global.print_header_string();



(**************************************************)
(* Recall the arguments *)
(**************************************************)


(**************************************************)
(* Parsing *)
(**************************************************)

(* Parsing the main input *)
print_message Verbose_standard ("Considering file " ^ !file ^ ".");
let parsing_structure = parser_lexer InputParser.main InputLexer.token !file in 

print_message Verbose_standard ("\nParsing done " ^ (after_seconds ()) ^ ".");


(**************************************************)
(* For each operation: *)
(**************************************************)

(* Count the number of operations *)
let debug_nb_operation = ref 0 in

let results =
List.map (fun parsed_operation -> 
	(* Increment *)
	incr debug_nb_operation;
	
	(**************************************************)
	(* Conversion to an abstract input *)
	(**************************************************)
	let abstract_input = 
	try (
		InputConverter.abstract_input_of_parsed_operation parsed_operation
	) with 
		| InputConverter.InvalidInput -> (print_error ("The input contains errors. Please check it again."); abort_program (); exit 0)
		| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease insult the developers."); abort_program (); exit 0)
		in

	print_message Verbose_low ("Operation " ^ (string_of_int !debug_nb_operation) ^ " checked and converted " ^ (after_seconds ()) ^ ".\n");
	(* Gc.major (); (*c'est quoi ca ? *) *)


	(**************************************************)
	(* Debug print: input *)
	(**************************************************)
	print_message Verbose_low ("\nInput:\n" ^ (InputPrinter.string_of_input abstract_input) ^ "\n");


	(**************************************************)
	(* PERFORM THE OPERATION *)
	(**************************************************)

	let string_of_nncc = LinearConstraint.string_of_nnconvex_constraint abstract_input.variable_names in
	let string_of_bool b = if b then "yes" else "no" in

	(* Add markers to parse the result easily *)
	let begin_marker = "BEGIN ANSWER" in
	let end_marker = "END ANSWER" in


	let rec perform_constraint = function
		| Op_and lc_list -> LinearConstraint.nnconvex_intersection_list (List.map perform_constraint lc_list)
		
		| Op_diff (c1, c2) -> LinearConstraint.nnconvex_difference (perform_constraint c1) (perform_constraint c2)
		
		| Op_hide (variables, lc) -> LinearConstraint.nnconvex_hide variables (perform_constraint lc)
		
		| Op_project (variables, lc) ->
			(* Compute variables to hide by negation *)
			let variables_to_hide = list_diff abstract_input.variables variables in
			LinearConstraint.nnconvex_hide variables_to_hide (perform_constraint lc)

		| Op_not lc -> LinearConstraint.negate (perform_constraint lc)
		
		| Op_simplify lc -> LinearConstraint.simplify (perform_constraint lc)
		
		| Op_time_elapsing (variables, lc) ->
			(* Find constant (non elapsing) variables *)
			let other_variables = list_diff abstract_input.variables variables in
			(* Call the time elapsing function *)
			LinearConstraint.nnconvex_time_elapse variables other_variables (perform_constraint lc)
			
		| Op_time_past (variables, lc) ->
			(* Find constant (non elapsing) variables *)
			let other_variables = list_diff abstract_input.variables variables in
			(* Call the time past function *)
			LinearConstraint.nnconvex_time_past variables other_variables (perform_constraint lc)
			
		| Op_union lc_list -> LinearConstraint.nnconvex_union_list (List.map perform_constraint lc_list)

		| Op_update (updates, lc) ->
			(* Call the dedicated function *)
			LinearConstraint.update updates (perform_constraint lc)
			
		| Op_zonepred(z1, z2, z, t, r) ->
			(* Find constant (non elapsing) variables *)
			let other_variables = list_diff abstract_input.variables t in
			(* Call the dedicated function *)
			LinearConstraint.nnconvex_constraint_zone_predecessor
				(perform_constraint z1)
				(perform_constraint z2)
				(perform_constraint z)
				t
				other_variables
				r
			
		(* zonepredgr(Zn-1, gn-1, Un-1, Zn, t, nont, gn, Un, Zn+1) *)
		| Op_zonepredgr(zn_minus_1, gn_minus_1, un_minus_1, zn, t, gn, un, zn_plus_1) ->
			(* Find constant (non elapsing) variables *)
			let other_variables = list_diff abstract_input.variables t in
			(* Call the dedicated function *)
			LinearConstraint.nnconvex_constraint_zone_predecessor_g_r
				(perform_constraint zn_minus_1)
				(perform_constraint gn_minus_1)
				un_minus_1
				(perform_constraint zn)
				t
				other_variables
				(perform_constraint gn)
				un
				(perform_constraint zn_plus_1)
			
		| Op_convex lc -> lc
	in

	let perform_bool = function
		| Op_equal (lc1, lc2) -> LinearConstraint.nnconvex_constraint_is_equal (perform_constraint lc1) (perform_constraint lc2)
		| Op_included (lc1, lc2) -> LinearConstraint.nnconvex_constraint_is_leq (perform_constraint lc1) (perform_constraint lc2)
		| Op_satisfiable lc -> not (LinearConstraint.nnconvex_constraint_is_false (perform_constraint lc))
	in

	let perform_oppoint = function
		| Op_exhibit lc -> LinearConstraint.nnconvex_constraint_exhibit_point (perform_constraint lc)
	in

	let result =
	(* First recall the operation in comments *)
	"(* OPERATION " ^ (string_of_int !debug_nb_operation) ^ ": \n"
	^ (InputPrinter.string_of_operation abstract_input.variable_names abstract_input.operation)
	^ "\n*)\n"
	^ begin_marker ^ "\n"
	
	^ (
	(* Solve and print *)
	match abstract_input.operation with
		| Op_bool b -> string_of_bool (perform_bool b)
		| Op_constraint c -> string_of_nncc (perform_constraint c)
		| Op_point op -> let result = try(
			InputPrinter.string_of_valuation abstract_input.variables abstract_input.variable_names (perform_oppoint op)
			) with LinearConstraint.EmptyConstraint ->
				print_warning "Empty constraint found in point operation";
				"ERROR! Empty constraint"
			in result
		| Op_nothing -> ("I am very proud to do nothing.")
	(* 	| _ -> raise (InternalError "not implemented") *)
	)
	
	^ "\n" ^ end_marker ^ "\n"
	
	in
	(**************************************************)
	(* Print the result *)
	(**************************************************)
	print_message Verbose_standard ("\nResult " ^ (string_of_int !debug_nb_operation) ^ ":\n");
	print_message Verbose_nodebug (result);

	(* Return result *)
	result

) parsing_structure
in


let result = string_of_list_of_string_with_sep "\n\n(*--------------------*)\n\n" results in



(**************************************************)
(* Write the result to file *)
(**************************************************)
write_to_file output_file_name result;


(**************************************************)
(* Bye bye! *)
(**************************************************)
terminate_program()
