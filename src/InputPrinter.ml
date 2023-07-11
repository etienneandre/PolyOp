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
 * Last modified: 2023/07/11
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


open Global
open AbstractInput
open LinearConstraint

(**************************************************)
(** Valuation *)
(**************************************************)

(* Convert a valuation into a string *)
let string_of_valuation variables variable_names valuation =
	string_of_list_of_string_with_sep
	", "
	(List.map
		(fun variable_index -> (variable_names variable_index) ^ "=" ^ (NumConst.string_of_numconst (valuation variable_index)) )
		variables
	)


(**************************************************)
(** Input *)
(**************************************************)

(* Convert an operation into a string *)
let string_of_operation variable_names operation =
	let convert_vars vars = string_of_list_of_string_with_sep ", " (List.map variable_names vars) in
	
	let convert_updates updates = "(" ^ (string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) -> (variable_names variable_index) ^ " := " ^ (LinearConstraint.string_of_linear_term variable_names linear_term) ) updates)) ^ ")" in
	
	let rec string_of_constraint = function
		| Op_and cp_list -> "and(" ^ (string_of_list_of_string_with_sep " , " (List.map string_of_constraint cp_list)) ^ ")"
		
		| Op_diff (c1, c2) -> "diff(" ^ (string_of_constraint c1) ^ " , " ^ (string_of_constraint c2) ^ ")"
		
		| Op_hide (vars, cp) -> "hide (" ^ (convert_vars vars) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		
		| Op_not c -> "not(" ^ (string_of_constraint c) ^ ")"
		
		| Op_simplify c -> "simplify(" ^ (string_of_constraint c) ^ ")"
		
		| Op_time_elapsing (vars, cp) -> "elapsing (" ^ (convert_vars vars) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		
		| Op_time_past (vars, cp) -> "past (" ^ (convert_vars vars) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		
		| Op_union cp_list -> "union(" ^ (string_of_list_of_string_with_sep " , " (List.map string_of_constraint cp_list)) ^ ")"

		| Op_update (updates, cp) -> "update (" ^ (convert_updates updates) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		
		| Op_zonepred (z1, z2, z, t, r) -> "zonepred ("
			^ (string_of_constraint z1) ^ ", "
			^ (string_of_constraint z2) ^ ", "
			^ (string_of_constraint z) ^ ", "
			^ (convert_vars t) ^ ", "
			^ (convert_vars r)
			^ ")"
		
		| Op_zonepredgr (zn_minus_1, gn_minus_1, un_minus_1, zn, t, gn, un, zn_plus_1) -> "zonepredgu ("
			^ (string_of_constraint zn_minus_1) ^ ", "
			^ (string_of_constraint gn_minus_1) ^ ", "
			^ (convert_updates un_minus_1) ^ ", "
			^ (string_of_constraint zn) ^ ", "
			^ (convert_vars t) ^ ", "
			^ (string_of_constraint gn) ^ ", "
			^ (convert_updates un) ^ ", "
			^ (string_of_constraint zn_plus_1)
			^ ")"
		
		| Op_convex cp -> LinearConstraint.string_of_nnconvex_constraint variable_names cp
	in
	
	let string_of_bool = function
		| Op_equal (cp1, cp2) -> "equal (" ^ (string_of_constraint cp1) ^ "), (" ^ (string_of_constraint cp2) ^ ")"
		| Op_included (cp1, cp2) -> "included (" ^ (string_of_constraint cp1) ^ ") in (" ^ (string_of_constraint cp2) ^ ")"
		| Op_satisfiable c -> "satisfiable(" ^ (string_of_constraint c) ^ ")"
	in
	
	let string_of_oppoint = function
		| Op_exhibit c -> "exhibit(" ^ (string_of_constraint c) ^ ")"
	in
	
	match operation with
		| Op_bool b -> string_of_bool b
		| Op_constraint c -> string_of_constraint c
		| Op_point op -> string_of_oppoint op
		| Op_nothing -> "nothing"
	

(* Convert a input into a string *)
let string_of_input input =
(*	(* Iterate and convert all operations to string *)
	string_of_list_of_string_with_sep "\n\n(**----------**)\n\n" (List.map (string_of_operation input.variable_names) input.operations)*)

	(* Iterate and convert all operations to string *)
	string_of_operation input.variable_names input.operation

