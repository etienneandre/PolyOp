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


(****************************************************************)
(** Names *)
(****************************************************************)

type variable_name = string



(****************************************************************)
(** Convex predicates and linear expressions *)
(****************************************************************)

(** Operators *)
type relop = OP_L | OP_LEQ | OP_EQ | OP_GEQ | OP_G


(** Linear expressions *)

type linear_term =
	| Constant of NumConst.t
	| Variable of NumConst.t * variable_name


type linear_expression =
	| Linear_term of linear_term
	| Linear_plus_expression of linear_expression * linear_term
	| Linear_minus_expression of linear_expression * linear_term


type linear_constraint =
	| True_constraint (** True *)
	| False_constraint (** False *)
	| Linear_constraint of linear_expression * relop * linear_expression


type convex_predicate = linear_constraint list

type nnconvex_predicate = convex_predicate list

(** Updates *)

type parsed_updates = (variable_name * linear_expression) list


(****************************************************************)
(** Input *)
(****************************************************************)

type parsed_op =
	| Parsop_bool of parsop_bool
	| Parsop_constraint of parsop_constraint
	| Parsop_point of parsop_point
	| Parsop_nothing
and parsop_bool =
	| Parsop_equal of parsop_constraint * parsop_constraint
	| Parsop_included of parsop_constraint * parsop_constraint
	| Parsop_satisfiable of parsop_constraint
and parsop_constraint =
	| Parsop_and of parsop_constraint list
	| Parsop_diff of parsop_constraint * parsop_constraint
	| Parsop_hide of variable_name list * parsop_constraint
	| Parsop_project of variable_name list * parsop_constraint
	| Parsop_not of parsop_constraint
	| Parsop_simplify of parsop_constraint
	| Parsop_time_elapsing of variable_name list * parsop_constraint
	| Parsop_time_past of variable_name list * parsop_constraint
	| Parsop_union of parsop_constraint list
	| Parsop_update of parsed_updates * parsop_constraint
	| Parsop_zonepred of parsop_constraint * parsop_constraint * parsop_constraint * variable_name list * variable_name list
	(* zonepredgr(Zn-1, gn-1, Un-1, Zn, t, gn, Un, Zn+1) *)
	| Parsop_zonepredgr of parsop_constraint * parsop_constraint * parsed_updates * parsop_constraint * variable_name list * parsop_constraint * parsed_updates * parsop_constraint
	| Parsop_convex of nnconvex_predicate
and parsop_point =
	| Parsop_exhibit of parsop_constraint

type parsing_structure = parsed_op list
