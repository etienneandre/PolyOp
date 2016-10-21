(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * École Centrale Nantes, France
 *
 * Author:        Etienne ANDRE
 * Created:       2011/04/27
 * Last modified: 2016/10/21
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
open AbstractStructure
open Arg
open ProgramPrinter



let string_error = "error"

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
let usage_msg = "Usage: PolyOp program_file [options]" in

(* Program file *)
let file = ref "" in

let nb_args = ref 0 in
			
(* Get the debug mode *)
let rec set_debug_mode_ref debug_mode =
		let mode = try debug_mode_of_string debug_mode
			with Not_found ->
			print_error ("The debug mode '" ^ debug_mode ^ "' is not valid.");
			Arg.usage speclist usage_msg;
			abort_program ();
			exit(0); in
		set_debug_mode mode

(* Options *)
and speclist = [
	("-debug", String set_debug_mode_ref, " Print more or less information. Can be set to 'nodebug', 'standard', 'low', 'medium', 'high', 'total'. Default: 'nodebug'");
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
		print_warning ("The program argument '" ^ arg ^ "' will be ignored.");
	)
) in

Arg.parse speclist anon_fun usage_msg;

(* Case no file *)
if !nb_args < 1 then(
	(* print_error ("Please give a source file name."); 
	Arg.usage speclist usage_msg;
	abort_program (); exit(0)*)
	file := "test.polyop"
);

(* Definition of output *)
let output_file_name = !file ^ ".res" in

(* If the program does not succeed: *)
write_to_file output_file_name string_error;


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

(* Parsing the main program *)
print_message Debug_standard ("Considering file " ^ !file ^ ".");
let parsing_structure = parser_lexer ProgramParser.main ProgramLexer.token !file in 

print_message Debug_standard ("\nParsing done " ^ (after_seconds ()) ^ ".");


(**************************************************)
(* Conversion to an abstract program *)
(**************************************************)

let program = 
try (
	ProgramConverter.abstract_program_of_parsing_structure
		parsing_structure
) with 
	| ProgramConverter.InvalidProgram -> (print_error ("The input program contains errors. Please check it again."); abort_program (); exit 0)
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease insult the developers."); abort_program (); exit 0)
	in

print_message Debug_standard ("Program checked and converted " ^ (after_seconds ()) ^ ".\n");
(* Gc.major (); (*c'est quoi ca ? *) *)


(**************************************************)
(* Debug print: program *)
(**************************************************)
print_message Debug_standard ("\nProgram:\n" ^ (ProgramPrinter.string_of_program program) ^ "\n");


(**************************************************)
(* PERFORM THE OPERATION *)
(**************************************************)
if program.operation = Op_nothing then(
	print_message Debug_standard ("\nI am very proud to do nothing.");
	terminate_program();
	exit 0
);





let string_of_nncc = LinearConstraint.string_of_nnconvex_constraint program.variable_names in
let string_of_bool b = if b then "yes" else "no" in


let rec perform_constraint = function
	| Op_and lc_list -> LinearConstraint.nnconvex_intersection_list (List.map perform_constraint lc_list)
	| Op_hide (variables, lc) -> LinearConstraint.nnconvex_hide variables (perform_constraint lc)
	| Op_not lc -> LinearConstraint.negate (perform_constraint lc)
	| Op_simplify lc -> LinearConstraint.simplify (perform_constraint lc)
	| Op_time_elapsing (variables, lc) ->
		(* Create the set of all variables *)
		let all_variables = list_of_interval 0 (program.nb_variables - 1) in
		(* Perform the intersection *)
		let other_variables = list_diff all_variables variables in
		(* Call the function *)
		LinearConstraint.nnconvex_time_elapse variables other_variables (perform_constraint lc)
	| Op_time_past (variables, lc) ->
		(* Create the set of all variables *)
		let all_variables = list_of_interval 0 (program.nb_variables - 1) in
		(* Perform the intersection *)
		let other_variables = list_diff all_variables variables in
		(* Call the function *)
		LinearConstraint.nnconvex_time_past variables other_variables (perform_constraint lc)
	| Op_convex lc -> lc
in
let perform_bool = function
	| Op_equal (lc1, lc2) -> LinearConstraint.nnconvex_constraint_is_equal (perform_constraint lc1) (perform_constraint lc2)
	| Op_included (lc1, lc2) -> LinearConstraint.nnconvex_constraint_is_leq (perform_constraint lc1) (perform_constraint lc2)
	| Op_satisfiable lc -> not (LinearConstraint.nnconvex_constraint_is_false (perform_constraint lc))
in

let result = match program.operation with
	| Op_bool b -> string_of_bool (perform_bool b)
	| Op_constraint c -> string_of_nncc (perform_constraint c)
	| Op_nothing -> print_error ("Internal error: the Op_nothing operation can't happen here.\nPlease insult the developers."); abort_program (); exit 0
(* 	| _ -> raise (InternalError "not implemented") *)
in


(**************************************************)
(* Print the result *)
(**************************************************)
print_message Debug_standard ("\nResult:\n");
print_message Debug_nodebug (result);


(**************************************************)
(* Write the result to file *)
(**************************************************)
write_to_file output_file_name result;


(**************************************************)
(* Bye bye! *)
(**************************************************)
terminate_program()
