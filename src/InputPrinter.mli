(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Author:        Étienne André
 * Created:       2011/04/27
 * Last modified: 2019/05/31
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


open AbstractInput



(**************************************************)
(** Valuation *)
(**************************************************)

(* Convert a valuation into a string *)
val string_of_valuation : (variable_index list) -> (variable_index -> variable_name) -> (variable_index -> NumConst.t) -> string


(**************************************************)
(** Input *)
(**************************************************)

(* Convert an operation into a string *)
val string_of_operation : (variable_index -> variable_name) -> operation -> string

(* Convert a input into a string *)
val string_of_input : abstract_input -> string

