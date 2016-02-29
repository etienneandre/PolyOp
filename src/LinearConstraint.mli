(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 *
 * Author:        Etienne ANDRE
 * Created:       2011/04/27
 * Last modified: 2011/04/27
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


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear inequality is pi0-compatible *)
val is_pi0_compatible_inequality : (variable -> coef) -> linear_inequality -> bool

(** Negate a linear inequality; for an equality, perform the pi0-compatible negation *)
val negate_wrt_pi0 : (variable -> coef) -> linear_inequality -> linear_inequality


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


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a constraint is false *)
val is_false : linear_constraint -> bool

(** Check if a constraint is true *)
val is_true : linear_constraint -> bool

(** Check if a constraint is satisfiable *)
val is_satisfiable : linear_constraint -> bool

(** Check if 2 constraints are equal *)
val is_equal : linear_constraint -> linear_constraint -> bool

(** Check if a constraint is included in another one *)
val is_leq : linear_constraint -> linear_constraint -> bool

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear constraint is pi0-compatible *)
val is_pi0_compatible : (variable -> coef) -> linear_constraint -> bool

(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
val partition_pi0_compatible : (variable -> coef) -> linear_constraint -> (linear_inequality list * linear_inequality list)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** makes a copy of a constraint *)
val copy : linear_constraint -> linear_constraint

(** Performs the intersection of a list of linear constraints *)
val intersection : linear_constraint list -> linear_constraint

(** Performs the intersection of a list of linear constraints with sideeffect *)
val intersection_assign : linear_constraint -> linear_constraint list -> unit

(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
val hide : variable list -> linear_constraint -> linear_constraint

(** Eliminate (using existential quantification) a set of variables in a linear constraint, with side effects *)
val hide_assign : variable list -> linear_constraint -> unit

(** 'rename_variables renaming_couples c' renames all variables according to the couples of the form (old, new) *)
val rename_variables : (variable * variable) list -> linear_constraint -> linear_constraint

(** 'rename_variables renaming_couples c' renames all variables according to the couples of the form (old, new), with side effects *)
val rename_variables_assign : (variable * variable) list -> linear_constraint -> unit

(** 'add_d d coef variables c' adds a variable 'coef * d' to any variable in 'variables' *)
val add_d : variable -> coef -> variable list -> linear_constraint -> linear_constraint

(** Minimize a linear constraints *)
val minimize : linear_constraint -> linear_constraint

(** Perform time elapsing on a set of variables: the first variable list will elapse, the second will remain constant *)
val time_elapse  : variable list -> variable list -> linear_constraint -> linear_constraint

(** Perform time elapsing on a set of variables; version with side effects *)
val time_elapse_assign  : variable list -> variable list -> linear_constraint -> unit


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear constraint into a string *)
val string_of_linear_constraint : (variable -> string) -> linear_constraint -> string

(** String for the false constraint *)
val string_of_false : string

(** String for the true constraint *)
val string_of_true : string

