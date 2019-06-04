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
 
(**************************************************)
(** {2 Exceptions} *)
(**************************************************)

exception EmptyConstraint


(**************************************************)
(** {2 Variables and coefficients} *)
(**************************************************)

type variable = int
type coef = NumConst.t

(**************************************************)
(** {2 Linear terms} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

type linear_term

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

val normalize_linear_term : linear_term -> Ppl_ocaml.linear_expression * NumConst.t

(** Create a linear term using a list of coef and variables, and a constant *)
val make_linear_term : (coef * variable) list -> coef -> linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Add two linear terms *)
val add_linear_terms : linear_term -> linear_term -> linear_term

(** Evaluate a linear term with a function assigning a value to each variable. *)
val evaluate_linear_term : (variable -> coef) -> linear_term -> coef


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear term into a string *)
val string_of_linear_term : (variable -> string) -> linear_term -> string


(**************************************************)
(** {2 Linear inequalities} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type op =
	| Op_g
	| Op_ge
	| Op_eq

type linear_inequality = Ppl_ocaml.linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear inequality using linear term and an operator *)
val make_linear_inequality : linear_term -> op -> linear_inequality

(*
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear inequality is pi0-compatible *)
val is_pi0_compatible_inequality : (variable -> coef) -> linear_inequality -> bool

(** Negate a linear inequality; for an equality, perform the pi0-compatible negation *)
val negate_wrt_pi0 : (variable -> coef) -> linear_inequality -> linear_inequality*)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear inequality into a string *)
val string_of_linear_inequality : (variable -> string) -> linear_inequality -> string


(**************************************************)
(** {2 Linear Constraints} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type linear_constraint = Ppl_ocaml.polyhedron


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear constraint from a list of linear inequalities *)
val make : linear_inequality list -> linear_constraint

(** 'set_manager int_dim real_dim' sets the constraint manager by giving the number of dimensions. *)
val set_manager : int -> int -> unit

(** Create a false constraint *)
val false_constraint : unit -> linear_constraint

(** Create a true constraint *)
val true_constraint : unit -> linear_constraint





(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Non-necessarily convex constraint on the parameters ("pointset powerset" in the underlying PPL implementation) *)
type nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a false constraint *)
val false_nnconvex_constraint : unit -> nnconvex_constraint

(** Create a true constraint *)
val true_nnconvex_constraint  : unit -> nnconvex_constraint

(** Copy a nnconvex_constraint *)
val nnconvex_copy : nnconvex_constraint -> nnconvex_constraint


(** Create a new nnconvex_constraint from a linear_constraint *)
val nnconvex_constraint_of_linear_constraint : linear_constraint -> nnconvex_constraint

(** Create a new non-convex nnconvex_constraint from a list of linear_constraint *)
val nnconvex_constraint_of_linear_constraints : linear_constraint list -> nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a nnconvex_constraint is false *)
val nnconvex_constraint_is_false : nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is true *)
val nnconvex_constraint_is_true  : nnconvex_constraint -> bool

(*(** Check if a nnconvex_constraint is pi0-compatible *)
val nnconvex_constraint_is_pi0_compatible : (variable -> coef) -> nnconvex_constraint -> bool*)

(** Check if a nnconvex_constraint is included in another one *)
val nnconvex_constraint_is_leq : nnconvex_constraint -> nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is equal to another one *)
val nnconvex_constraint_is_equal : nnconvex_constraint -> nnconvex_constraint -> bool


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Operations without modification} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Exhibit a point in a nnconvex_constraint; raise EmptyConstraint if the constraint is empty. *)
val nnconvex_constraint_exhibit_point : nnconvex_constraint -> (variable -> coef)

(** Given two zones z1 and z2, such that z2 is the successor of z1, and given z a subset of z2, then nnconvex_constraint_zone_predecessor z1 z2 z t nott r computes the zone predecessor of z within z1, given the set t (nott) of variables sensitive (resp. insensitive) to time-elapsing, and r the variables reset between z1 and z2. *)
val nnconvex_constraint_zone_predecessor : nnconvex_constraint -> nnconvex_constraint -> nnconvex_constraint -> (variable list) -> (variable list) -> (variable list) -> nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Modifications} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Simplify a constraint by applying pairwaise-reduction and omega-reduction; version with copy (argument is not modified) *)
val simplify : nnconvex_constraint -> nnconvex_constraint

(** Negate a nnconvex_constraint *)
val negate : nnconvex_constraint -> nnconvex_constraint

(** Performs the intersection of a nnconvex_constraint with a linear_constraint; the first nnconvex_constraint is modified, the second is not *)
val nnconvex_intersection_assign : nnconvex_constraint -> nnconvex_constraint -> unit

(** Performs the intersection of a nnconvex_constraint with a linear_constraint and return a new nnconvex_constraint (none of the arguments is modified) *)
val nnconvex_intersection : nnconvex_constraint -> nnconvex_constraint -> nnconvex_constraint

(** Performs the intersection of a list of nnconvex_constraint and return a new nnconvex_constraint (none of the arguments is modified) *)
val nnconvex_intersection_list : nnconvex_constraint list -> nnconvex_constraint


(** Performs the union of a nnconvex_constraint with a linear_constraint; the nnconvex_constraint is modified, the linear_constraint is not *)

(** Performs the union of a nnconvex_constraint with a linear_constraint; the nnconvex_constraint is modified, the linear_constraint is not *)
val nnconvex_union_with_linear_constraint : nnconvex_constraint -> linear_constraint -> unit

(** Performs the union of a nnconvex_constraint with another nnconvex_constraint; the first nnconvex_constraint is modified, the second is not *)
val nnconvex_union : nnconvex_constraint -> nnconvex_constraint -> unit


(** Performs the difference between a first px_nnconvex_constraint and a second px_nnconvex_constraint; the first is modified, the second is not *)
val nnconvex_difference : nnconvex_constraint -> nnconvex_constraint -> nnconvex_constraint

(** Eliminate a set of variables *)
val nnconvex_hide : variable list -> nnconvex_constraint -> nnconvex_constraint

(** Apply time elapsing to an nnconvex_constraint with variable_elapse elapsing, and variable_constant remaining constant *)
val nnconvex_time_elapse : variable list -> variable list -> nnconvex_constraint -> nnconvex_constraint

(** Time elapsing function, in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) *)
val nnconvex_time_past : variable list -> variable list -> nnconvex_constraint -> nnconvex_constraint



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to a list of linear_constraint} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** Converts a nnconvex_constraint into a list of linear_constraint such that the union of this list is equal to the nnconvex_constraint *)
val linear_constraint_list_of_nnconvex_constraint : nnconvex_constraint -> linear_constraint list


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a nnconvex_constraint into a string *)
val string_of_nnconvex_constraint : (variable -> string) -> nnconvex_constraint -> string

