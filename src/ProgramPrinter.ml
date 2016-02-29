(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 *
 * Author:        Etienne ANDRE
 * Created:       2011/04/27
 * Last modified: 2011/05/30
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
open AbstractStructure
open LinearConstraint

(**************************************************)
(** Program *)
(**************************************************)

(* Convert a program into a string *)
let string_of_program program =
	let convert_cp = string_of_linear_constraint program.variable_names in
	let convert_vars vars = string_of_list_of_string_with_sep ", " (List.map program.variable_names vars) in
	
	let rec string_of_constraint = function
		| Op_and cp_list -> "and(" ^ (string_of_list_of_string_with_sep " , " (List.map string_of_constraint cp_list)) ^ ")"
		| Op_hide (vars, cp) -> "hide (" ^ (convert_vars vars) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		| Op_simplify c -> "simplify(" ^ (string_of_constraint c) ^ ")"
		| Op_time_elapsing (vars, cp) -> "elapsing (" ^ (convert_vars vars) ^ ") in (" ^ (string_of_constraint cp) ^ ")"
		| Op_convex cp -> convert_cp cp
	in
	let string_of_bool = function
		| Op_equal (cp1, cp2) -> "equal (" ^ (string_of_constraint cp1) ^ "), (" ^ (string_of_constraint cp2) ^ ")"
		| Op_included (cp1, cp2) -> "included (" ^ (string_of_constraint cp1) ^ ") in (" ^ (string_of_constraint cp2) ^ ")"
		| Op_satisfiable c -> "satisfiable(" ^ (string_of_constraint c) ^ ")"
	in
	
	match program.operation with
		| Op_bool b -> string_of_bool b
		| Op_constraint c -> string_of_constraint c
		| Op_nothing -> "nothing"
	


