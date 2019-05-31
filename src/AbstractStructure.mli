(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * École Centrale Nantes, France
 * Université Paris 13, LIPN, CNRS, France
 *
 * Author:        Étienne André
 * Created:       2011/04/27
 * Last modified: 2017/03/21
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



(****************************************************************)
(* Modules *)
(****************************************************************)
open Global



type variable_index	= int
type variable_name	= string


(****************************************************************)
(** The actual operation *)
(****************************************************************)
type operation =
	| Op_bool of op_bool
	| Op_constraint of op_constraint
	| Op_nothing
and op_bool =
	| Op_equal of op_constraint * op_constraint
	| Op_included of op_constraint * op_constraint
	| Op_satisfiable of op_constraint
and op_constraint =
	| Op_and of op_constraint list
	| Op_diff of op_constraint * op_constraint
	| Op_hide of variable_index list * op_constraint
	| Op_not of op_constraint
	| Op_simplify of op_constraint
	| Op_time_elapsing of variable_index list * op_constraint
	| Op_time_past of variable_index list * op_constraint
	| Op_convex of LinearConstraint.nnconvex_constraint



(****************************************************************)
(** The abstract program *)
(****************************************************************)
type abstract_program = {
	(* Cardinality *)
	nb_variables : int;

	(* Names of the variable *)
	variable_names : variable_index -> variable_name;

	(* The operation to perform *)
	operation : operation;
}


