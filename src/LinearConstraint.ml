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
 * Last modified: 2019/06/04
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
(*open Apron   *)
(*open Lincons0*)

module Ppl = Ppl_ocaml
open Ppl

open Global
open Gmp.Z.Infixes

(**************************************************)
(* TYPES *)
(**************************************************)

type variable = int
type coef = NumConst.t

(*type linear_term = Linexpr0.t*)

(* For legacy reasons (rational coefficients in input),      *)
(* the linear_term is a generalization of the corresponding  *)
(* PPL data structure Ppl.linear_expression, using rationals *)
(* instead of integers. *)
type linear_term =
	  Var of variable
	| Coef of coef
	| Pl of linear_term * linear_term
	| Mi of linear_term * linear_term
	| Ti of coef * linear_term

type op =
	| Op_g
	| Op_ge
	| Op_eq

(*type linear_inequality = Lincons0.t*)
type linear_inequality = Ppl.linear_constraint

(*type linear_constraint = Polka.strict Polka.t Abstract0.t *)
type linear_constraint = Ppl.polyhedron


	
(**************************************************)
(** {2 Exceptions} *)
(**************************************************)

exception EmptyConstraint


(**************************************************)
(** Global variables *)
(**************************************************)

(* The manager *)
(*let manager = Polka.manager_alloc_strict ()*)

(* The number of integer dimensions *)
let int_dim = ref 0

(* The number of real dimensions *)
let real_dim = ref 0

(* Total number of dimensions *)
let total_dim = ref 0



(**************************************************)
(* Useful Functions *)
(**************************************************)

(** check the dimensionality of a polyhedron *)
let assert_dimensions poly =
	let ndim = ppl_Polyhedron_space_dimension poly in
	if not (ndim = !total_dim) then (
		print_error ("Polyhedron has too few dimensions (" ^ (string_of_int ndim) ^ " / " ^ (string_of_int !total_dim) ^ ")");
		raise (InternalError "Inconsistent polyhedron found")	
	)			 	

(**************************************************)
(** {2 Linear terms} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let make_linear_term members coef =
	List.fold_left (fun term head ->
		let (c, v) = head in 
			if c = NumConst.one then
				Pl (Var v, term)
			else
				Pl ((Ti (c, Var v), term))
	)	(Coef coef) members


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Add two linear terms *)
let add_linear_terms lt1 lt2 =
	Pl (lt1, lt2)


(*--------------------------------------------------*)
(* Evaluation *)
(*--------------------------------------------------*)

(** Evaluate a linear term with a function assigning a value to each variable. *)
let rec evaluate_linear_term valuation_function linear_term =
	match linear_term with
		| Coef c -> c
		| Var v -> (
			  try valuation_function v 
			  with _ -> raise(InternalError ("No value was found for variable " ^ (string_of_int v) ^ ", while trying to evaluate a linear term; this variable was probably not defined.")))
		| Pl (lterm, rterm) -> ( 
				let lval = evaluate_linear_term valuation_function rterm in
				let rval = evaluate_linear_term valuation_function lterm in
				NumConst.add lval rval)
		| Mi (lterm, rterm) -> (
				let lval = evaluate_linear_term valuation_function rterm in
				let rval = evaluate_linear_term valuation_function lterm in
				NumConst.sub lval rval)
		| Ti (fac, rterm) -> ( 
				let rval = evaluate_linear_term valuation_function rterm in
				NumConst.mul fac rval)


(** Evaluate a linear term (PPL) with a function assigning a value to each variable. *)
let rec evaluate_linear_term_ppl valuation_function linear_term =
	match linear_term with
		| Coefficient z -> NumConst.numconst_of_mpz z
		| Variable v -> (
			  try valuation_function v 
			  with _ -> raise(InternalError ("No value was found for variable " ^ (string_of_int v) ^ ", while trying to evaluate a linear term; this variable was probably not defined.")))
	  | Unary_Plus t -> evaluate_linear_term_ppl valuation_function t
		| Unary_Minus t -> NumConst.neg (evaluate_linear_term_ppl valuation_function t)
		| Plus (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.add lval rval)
	 | Minus (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.sub lval rval)
	 | Times (z, rterm) -> ( 
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.mul (NumConst.numconst_of_mpz z) rval)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let string_of_coef = NumConst.string_of_numconst
let string_of_constant = NumConst.string_of_numconst


(** Convert a linear term into a string *)	
let rec string_of_linear_term names linear_term =
	match linear_term with
		| Coef c -> string_of_coef c
		| Var v -> names v
		| Pl (lterm, rterm) -> (
			  let lstr = string_of_linear_term names lterm in
				let rstr = string_of_linear_term names rterm in
				lstr ^ " + " ^ rstr )
		| Mi (lterm, rterm) -> (
			  let lstr = string_of_linear_term names lterm in
				let rstr = string_of_linear_term names rterm in
				lstr ^ " - (" ^ rstr ^ ")" )
		| Ti (fac, rterm) -> (
				let fstr = string_of_coef fac in
				let tstr = string_of_linear_term names rterm in
				match rterm with
					| Coef _ -> fstr ^ "*" ^ tstr
					| Var  _ -> fstr ^ "*" ^ tstr
					| _ -> fstr ^ " * (" ^ tstr ^ ")" ) 				


(** Convert a linear term (PPL) into a string *)								
let rec string_of_linear_term_ppl names linear_term =
	match linear_term with
		| Coefficient z -> Gmp.Z.string_from z
		| Variable v -> names v
		| Unary_Plus t -> string_of_linear_term_ppl names t
		| Unary_Minus t -> (
				let str = string_of_linear_term_ppl names t in
				"-(" ^ str ^ ")")
		| Plus (lterm, rterm) -> (
			  let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " + " ^ rstr )
		| Minus (lterm, rterm) -> (
			  let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " - (" ^ rstr ^ ")" )
		| Times (z, rterm) -> (
				let fstr = Gmp.Z.string_from z in
				let tstr = string_of_linear_term_ppl names rterm in
				if (Gmp.Z.equal z (Gmp.Z.one)) then
					tstr
				else 
					match rterm with
						| Coefficient _ -> fstr ^ "*" ^ tstr
						| Variable    _ -> fstr ^ "*" ^ tstr
						| _ -> fstr ^ " * (" ^ tstr ^ ")" ) 				
				

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to PPL} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(* In order to convert a linear_term (with rational coefficients) *)
(* to the corresponding PPL data structure (linear_expression), it is normalized such *)
(* that the only non-rational coefficient is outside the term:    *)
(* p/q * ( ax + by + c ) *)
let rec normalize_linear_term (lt : linear_term) : (Ppl.linear_expression * NumConst.t) =
		match lt with
			| Var v -> Variable v, NumConst.one
			| Coef c -> (
				  let p = NumConst.get_num c in
				  let q = NumConst.get_den c in
				  Coefficient p, NumConst.numconst_of_zfrac Gmp.Z.one q )
			| Pl (lterm, rterm) -> (
					let lterm_norm, fl = normalize_linear_term lterm in
					let rterm_norm, fr = normalize_linear_term rterm in
					let pl = NumConst.get_num fl in
					let ql = NumConst.get_den fl in
					let pr = NumConst.get_num fr in
					let qr = NumConst.get_den fr in
					(Plus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
					NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
			| Mi (lterm, rterm) -> (
					let lterm_norm, fl = normalize_linear_term lterm in
					let rterm_norm, fr = normalize_linear_term rterm in
					let pl = NumConst.get_num fl in
					let ql = NumConst.get_den fl in
					let pr = NumConst.get_num fr in
					let qr = NumConst.get_den fr in
					(Minus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
					NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
			| Ti (fac, term) -> (
					let term_norm, r = normalize_linear_term term in
					let p = NumConst.get_num fac in
					let q = NumConst.get_den fac in
					term_norm, NumConst.mul r (NumConst.numconst_of_zfrac p q))				


(** Convert our ad-hoc linear_term into a Ppl.linear_expression *)
let ppl_linear_expression_of_linear_term linear_term : Ppl.linear_expression =
	let ppl_term, r = normalize_linear_term linear_term in
	let p = NumConst.get_num r in
	(* Simplifies a bit *)
	if Gmp.Z.equal p Gmp.Z.one then ppl_term
	else Times (p, ppl_term)


(**************************************************)
(** {2 Linear inequalities} *)
(**************************************************)

(************** TO DO : minimize inequalities as soon as they have been created / changed *)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(* Functions *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear inequality using a linear term and an operator *)
let make_linear_inequality linear_term op =
	(* Convert to Ppl.linear_expression *)
	let linear_expression = ppl_linear_expression_of_linear_term linear_term in
	(* Build zero term for comparison with the operator *)
	let zero_term = Coefficient Gmp.Z.zero in
	match op with
		| Op_g -> Greater_Than (linear_expression, zero_term)
		| Op_ge -> Greater_Or_Equal (linear_expression, zero_term)
		| Op_eq -> Equal (linear_expression, zero_term)
	

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** split a linear inequality into its two terms and the operator *)
let split_linear_inequality = function
	| Less_Than (lterm, rterm) -> lterm, rterm, Less_Than_RS
	| Less_Or_Equal (lterm, rterm) -> lterm, rterm, Less_Or_Equal_RS
	| Equal (lterm, rterm) -> lterm, rterm, Equal_RS
	| Greater_Than (lterm, rterm) -> lterm, rterm, Greater_Than_RS
	| Greater_Or_Equal (lterm, rterm) -> lterm, rterm, Greater_Or_Equal_RS
	
(** build a linear inequality from two terms and an operator *)
let build_linear_inequality lterm rterm op = 
	match op with
		| Less_Than_RS -> Less_Than (lterm, rterm)
		| Less_Or_Equal_RS -> Less_Or_Equal (lterm, rterm)
		| Equal_RS -> Equal (lterm, rterm)
		| Greater_Than_RS -> Greater_Than (lterm, rterm)
		| Greater_Or_Equal_RS -> Greater_Or_Equal (lterm, rterm)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)				   	


let is_zero_coef = function
	| Coefficient c -> c =! Gmp.Z.zero
	| _ -> false


(** build a sum of two expressions; respects the case where one of the 
	  operands is zero *)
let compact_sum lexpr rexpr =
	if is_zero_coef lexpr then (
		rexpr
  ) else (
		if is_zero_coef rexpr then (
			lexpr
		) else (
			Plus (lexpr, rexpr)
		))

(** splits an expression into positive and negative part for pretty printing;
 	  an expression a-b is mapped to (a, b) *) 
let rec sign_split_expression = function
	| Coefficient c ->
		if c <! Gmp.Z.zero then (
			(Coefficient Gmp.Z.zero, Coefficient (Gmp.Z.neg c))
		) else (
			(Coefficient c, Coefficient Gmp.Z.zero)
		)
	| Variable v -> (Variable v, Coefficient Gmp.Z.zero)
	| Unary_Plus expr -> sign_split_expression expr
	| Unary_Minus expr ->
		let pos, neg = sign_split_expression expr in (neg, pos)
	| Plus (lexpr, rexpr) -> 
		let lpos, lneg = sign_split_expression lexpr in
		let rpos, rneg = sign_split_expression rexpr in
		let new_pos = compact_sum lpos rpos in 
		let new_neg = compact_sum lneg rneg in 
		(new_pos, new_neg)
	| Minus (lexpr, rexpr) -> 
		sign_split_expression (Plus (lexpr, Unary_Minus rexpr))
	| Times (c, expr) -> 
		let pos, neg = sign_split_expression expr in
		let invert = c <! Gmp.Z.zero in
		let new_c = if invert then Gmp.Z.neg c else c in
		if new_c =! Gmp.Z.one then (
			if invert then (neg, pos) else (pos, neg)
		) else (
			let new_pos = if is_zero_coef pos then Coefficient Gmp.Z.zero else Times (new_c, pos) in
			let new_neg = if is_zero_coef neg then Coefficient Gmp.Z.zero else Times (new_c, neg) in
			if invert then (new_neg, new_pos) else (new_pos, new_neg)
		)			


(** normalize an inequality for pretty printing; *)
(** the expressions are rearranged such that only posistive coefficients occur *)
let normalize_inequality ineq = 
	let lterm, rterm, op = split_linear_inequality ineq in
	let lpos, lneg = sign_split_expression lterm in
	let rpos, rneg = sign_split_expression rterm in
	let lnew = compact_sum lpos rneg in
	let rnew = compact_sum rpos lneg in
	build_linear_inequality lnew rnew op


(** Convert a linear inequality into a string *)
let string_of_linear_inequality names linear_inequality =
	let normal_ineq = normalize_inequality linear_inequality in
	let lterm, rterm, op = split_linear_inequality normal_ineq in
	let lstr = string_of_linear_term_ppl names lterm in
	let rstr = string_of_linear_term_ppl names rterm in	
	let opstr = match op with
		| Less_Than_RS -> " < "
		| Less_Or_Equal_RS -> " <= "
		| Equal_RS -> " = "
		| Greater_Than_RS -> " > "
		| Greater_Or_Equal_RS -> " >= " in
	lstr ^ opstr ^ rstr


(**************************************************)
(** {2 Linear Constraints} *)
(**************************************************)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a false constraint *)
let false_constraint () =	
	ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Empty

(** Create a true constraint *)
let true_constraint () = 
	ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Universe

(** Create a linear constraint from a list of linear inequalities *)
let make inequalities = 
	let poly = true_constraint () in
	ppl_Polyhedron_add_constraints poly inequalities;
	assert_dimensions poly;
  poly
	
(** Set the constraint manager *)
let set_manager int_d real_d =
	int_dim := int_d;
	real_dim := real_d;
	total_dim := int_d + real_d 


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a constraint is false *)
let is_false = ppl_Polyhedron_is_empty

(** Check if a constraint is true *)
let is_true = ppl_Polyhedron_is_universe

(** Check if a constraint is satisfiable *)
let is_satisfiable = fun c -> not (is_false c)

(** Check if 2 constraints are equal *)
let is_equal = ppl_Polyhedron_equals_Polyhedron

(** Check if a constraint is included in another one *)
let is_leq x y = ppl_Polyhedron_contains_Polyhedron y x

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Accesss} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(* Return the list of inequalities that build the polyhedron (interface to PPL) *)
let get_inequalities = ppl_Polyhedron_get_constraints


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** String for the false constraint *)
let string_of_false = "false"


(** String for the true constraint *)
let string_of_true = "true"


(** Convert a linear constraint into a string *)
let string_of_linear_constraint names linear_constraint =
	(* First check if true *)
	if is_true linear_constraint then string_of_true
	(* Then check if false *)
	else if is_false linear_constraint then string_of_false
	else
	(* Get an array of linear inequalities *)
	let list_of_inequalities = get_inequalities linear_constraint in
	let array_of_inequalities = Array.of_list list_of_inequalities in
	"  " ^
	(string_of_array_of_string_with_sep
		"\n& "
		(Array.map (string_of_linear_inequality names) array_of_inequalities)
	)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let copy linear_constraint =
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint

(** Performs the intersection of a list of linear constraints *)
let intersection linear_constraints =
	let result_poly = true_constraint () in
	List.iter (fun poly -> ppl_Polyhedron_intersection_assign result_poly poly) linear_constraints;
	assert_dimensions result_poly;
	result_poly	
	
(** same function, with side effects *)
let intersection_assign linear_constraint constrs =
	List.iter (fun poly -> ppl_Polyhedron_intersection_assign linear_constraint poly) constrs;
	assert_dimensions linear_constraint


(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide variables linear_constraint =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then (
		List.iter (fun v -> print_message Verbose_high ("hide v" ^ string_of_int v)) variables;
	);
	(* copy polyhedron, as PPL function has sideeffects *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	ppl_Polyhedron_unconstrain_space_dimensions poly variables;
	assert_dimensions poly;
	poly
	
	
(** Eliminate a set of variables, side effects version *)
let hide_assign variables linear_constraint =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then (
		List.iter (fun v -> print_message Verbose_high ("hide v" ^ string_of_int v)) variables;
	);
	ppl_Polyhedron_unconstrain_space_dimensions linear_constraint variables;
	assert_dimensions linear_constraint


(** rename variables in a constraint *)
let rename_variables list_of_couples linear_constraint =
	(* copy polyhedron, as ppl function has sideeffects *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	(* add reverse mapping *)
	let reverse_couples = List.map (fun (a,b) -> (b,a)) list_of_couples in
	let joined_couples = List.rev_append list_of_couples reverse_couples in
	(* find all dimensions that will be mapped *)
	let from, _  = List.split joined_couples in
	(* add identity pairs (x,x) for remaining dimensions *) 
	let rec add_id list i = 
		if i < 0 then list else
			if not (List.mem i from) then
				(i,i) :: add_id list (i-1)
			else
				add_id list (i-1)
		in 
	let complete_list = add_id joined_couples (!total_dim - 1) in
  (* Print some information *)
	if verbose_mode_greater Verbose_high then (
		let ndim = ppl_Polyhedron_space_dimension poly in
		print_message Verbose_high ("mapping space dimensions, no. dimensions is " ^ string_of_int ndim);
		List.iter (fun (a,b) -> (print_message Verbose_high ("map v" ^ string_of_int a ^ " -> v" ^ string_of_int b))) complete_list;
	);
	(* perfom the mapping *)
	ppl_Polyhedron_map_space_dimensions poly complete_list;
	assert_dimensions poly;
	poly

				
(** rename variables in a constraint, with side effects *)
let rename_variables_assign list_of_couples linear_constraint =
	(* add reverse mapping *)
	let reverse_couples = List.map (fun (a,b) -> (b,a)) list_of_couples in
	let joined_couples = List.rev_append list_of_couples reverse_couples in
	(* find all dimensions that will be mapped *)
	let from, _  = List.split joined_couples in
	(* add identity pairs (x,x) for remaining dimensions *) 
	let rec add_id list i = 
		if i < 0 then list else
			if not (List.mem i from) then
				(i,i) :: add_id list (i-1)
			else
				add_id list (i-1)
		in 
	let complete_list = add_id joined_couples (!total_dim - 1) in
  (* Print some information *)
	if verbose_mode_greater Verbose_high then (
		let ndim = ppl_Polyhedron_space_dimension linear_constraint in
		print_message Verbose_high ("mapping space dimensions, no. dimensions is " ^ string_of_int ndim);
		List.iter (fun (a,b) -> (print_message Verbose_high ("map v" ^ string_of_int a ^ " -> v" ^ string_of_int b))) complete_list;
	);
	(* perfom the mapping *)
	ppl_Polyhedron_map_space_dimensions linear_constraint complete_list;
	assert_dimensions linear_constraint
				
				
(** substitutes all variables in a linear term.
		The substitution is given as a function sub: var -> linear_term *)
let rec substitute_variables_in_term sub linear_term =
	match linear_term with		
		| Coefficient z -> Coefficient z
		| Variable v -> sub v
		| Unary_Plus t -> Unary_Plus t
		| Unary_Minus t -> Unary_Minus t
		| Plus (lterm, rterm) -> (
				Plus (substitute_variables_in_term sub lterm,
							substitute_variables_in_term sub rterm))
		| Minus (lterm, rterm) -> (
				Minus (substitute_variables_in_term sub lterm,
							 substitute_variables_in_term sub rterm))
		| Times (z, rterm) -> (
				Times (z, substitute_variables_in_term sub rterm))

		
(** substitutes all variables in a linear inequality *)
let substitute_variables sub linear_inequality =
	match linear_inequality with
		| Less_Than (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Less_Than (lsub, rsub))
		| Less_Or_Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Less_Or_Equal (lsub, rsub))
		| Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Equal (lsub, rsub))
		| Greater_Than (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Greater_Than (lsub, rsub))
		| Greater_Or_Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Greater_Or_Equal (lsub, rsub))
		

let split_q r = 
	let p = NumConst.get_num r in
	let q = NumConst.get_den r in
	p, q

let add_d d coef variable_list linear_constraint =
	(* get numerator and denominator of rational coefficient *)
	let p, q = split_q coef in 
	(* function for building the affine translation of a variable: v -> v + coef*d *)
	let affine_translation = fun v -> Plus (Times (q, Variable v), Times (p, Variable d)) in
	(* copy linear constraint, as PPL functions have side effects *)	
	let result_poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	(* perform the affine translations *)
	List.iter (fun v -> 
		ppl_Polyhedron_affine_preimage result_poly v (affine_translation v) q
	) variable_list;
	assert_dimensions result_poly;
	result_poly


let minimize linear_constraint =
	(* Get a list of minimized linear inequalities *)
	let list_of_inequalities = ppl_Polyhedron_get_minimized_constraints linear_constraint in
	make list_of_inequalities


(*------------------------------------------------------------*)
(* Time elapsing and time past *)
(*------------------------------------------------------------*)

(* Generic time elapsing function *)
(* 'reverse_direction' should be minus_one for growing, one for decreasing *)
let time_elapse_gen_assign reverse_direction variables_elapse variables_constant linear_constraint =
	(* Create the inequalities var = 1, for var in variables_elapse *)
	let inequalities_elapse = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] reverse_direction in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variables_elapse in
	(* Create the inequalities var = 0, for var in variables_constant *)
	let inequalities_constant = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variables_constant in
	(* Convert both sets of inequalities to a constraint *)
	let linear_constraint_time = make (List.rev_append inequalities_elapse inequalities_constant) in
	
	(* Apply the time elapsing using PPL *)
	ppl_Polyhedron_time_elapse_assign linear_constraint linear_constraint_time


(** Time elapsing function *)
let time_elapse_assign = time_elapse_gen_assign NumConst.minus_one



let time_elapse variables_elapse variables_constant linear_constraint =
	let linear_constraint = copy linear_constraint in
	time_elapse_assign variables_elapse variables_constant linear_constraint;
	linear_constraint


(** Time elapsing function, in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) *)
let time_past_assign variables_elapse variables_constant linear_constraint =
	(* 1) Apply generic function *)
	time_elapse_gen_assign NumConst.one variables_elapse variables_constant linear_constraint;
	
	(* 2) Constrain the elapsing variables to be non-negative! *)
	(* Create the inequalities var >= 0, for var in variables_elapse *)
	let inequalities_nonnegative = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_ge
	) variables_elapse in
	(* Take intersection *)
	intersection_assign linear_constraint [(make inequalities_nonnegative)]
	


(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Non-necessarily convex constraint on the parameters ("pointset powerset" in the underlying PPL implementation) *)
type nnconvex_constraint = Ppl.pointset_powerset_nnc_polyhedron


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a false constraint *)
let false_nnconvex_constraint () = ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension !total_dim Empty


(** Create a true constraint *)
let true_nnconvex_constraint () = ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension !total_dim Universe


(** Create a new nnconvex_constraint from a linear_constraint *)
let nnconvex_constraint_of_linear_constraint = ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron


(** Copy a nnconvex_constraint *)
let nnconvex_copy = ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron




(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Get the list of linear_constraint the disjunction of which makes a nnconvex_constraint *)
let get_disjuncts nnconvex_constraint =
	(* Create ref for the result *)
	let disjuncts = ref [] in

	(* Create iterator *)
	let iterator = ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator nnconvex_constraint in
	(* Create an iterator for the end *)
	let end_iterator = ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator nnconvex_constraint in
	
	(* Iterate until the end *)
	(*** NOTE: apparently, ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator represents the index AFTER the last element, hence the following test is correct ***)
	while not (ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator iterator end_iterator) do
		(* Get the current disjunct *)
		let disjunct = ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct iterator in
		
		(* Add it to the list of disjuncts *)
		disjuncts := disjunct :: !disjuncts;
		
		(* Increment the iterator *)
		ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator iterator;
	done;
	
	(* Return disjuncts *)
	List.rev (!disjuncts)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a nnconvex_constraint is false *)
let nnconvex_constraint_is_false = ppl_Pointset_Powerset_NNC_Polyhedron_is_empty


(** Check if a nnconvex_constraint is true *)
let nnconvex_constraint_is_true = ppl_Pointset_Powerset_NNC_Polyhedron_is_universe



(*(** Check if a nnconvex_constraint is pi0-compatible *)
(*** NOTE: here, we split the nnconvex_constraint into a list of convex constraints, and we perform the check; the other option would have been to create a nnconvex_constraint from the point, and check inclusion ***)
(*** WARNING: function not tested ***)
let nnconvex_constraint_is_pi0_compatible pval nnconvex_constraint =
	(* 1) Get the constraints *)
	let disjuncts = get_disjuncts nnconvex_constraint in
	
	(* 2) Check each of them *)
	List.exists (fun linear_constraint -> is_pi0_compatible pval linear_constraint) disjuncts*)



(** Check if a nnconvex_constraint is included in another one *)
let nnconvex_constraint_is_leq nnconvex_constraint nnconvex_constraint' =
	(*** NOTE: PPL works in the reverse order: the 2nd contains the 1st one ***)
	(*** NOTE: ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron Returns <CODE>true</CODE> if and only if each disjunct of p y is contained in a disjunct of p' ***)
	ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron nnconvex_constraint' nnconvex_constraint


(** Check if a nnconvex_constraint is equal to another one *)
let nnconvex_constraint_is_equal =
	(*** NOTE: ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron does check that each disjunct equals another disjunct ***)
	ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Simplification} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** Simplify a constraint by applying pairwaise-reduction and omega-reduction *)
let simplify_assign nnconvex_constraint =
	ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce nnconvex_constraint;
	ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce nnconvex_constraint;
	()

(** Simplify a constraint by applying pairwaise-reduction and omega-reduction; version with copy (argument is not modified) *)
let simplify nnconvex_constraint =
	(* First copy *)
	let copy = nnconvex_copy nnconvex_constraint in
	(* Perform simplification with side effects *)
	simplify_assign copy;
	(* Return copy *)
	copy


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a nnconvex_constraint into a string *)
let string_of_nnconvex_constraint names nnconvex_constraint =
	(* First reduce (avoids identical disjuncts) *)
	simplify_assign nnconvex_constraint;
	
	(* Get the disjuncts *)
	let disjuncts = get_disjuncts nnconvex_constraint in
	
	(* Case false *)
	if disjuncts = [] then string_of_false else(
	
		(* Convert each disjunct into a string *)
		let disjuncts_string = List.map (string_of_linear_constraint names) disjuncts in
		
		(* Concatenate using an "OR" *)
		string_of_list_of_string_with_sep "\nor\n " disjuncts_string
	)




(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Operations} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)



(*	(*** NOTE: version nnconvex_constraint ^ linear_constraint ***)
(** Performs the intersection of a nnconvex_constraint with a linear_constraint; the nnconvex_constraint is modified, the linear_constraint is not *)
let nnconvex_intersection nnconvex_constraint linear_constraint =
(*	(* Statistics *)
	ppl_nb_is_true := !ppl_nb_is_true + 1;
	let start = Unix.gettimeofday() in*)
	(* First retrieve inequalities *)
	let constraint_system =  get_inequalities linear_constraint in
	(* Actual call to PPL *)
	ppl_Pointset_Powerset_NNC_Polyhedron_add_constraints nnconvex_constraint constraint_system;
(*	(* Statistics *)
	ppl_t_is_true := !ppl_t_is_true +. (Unix.gettimeofday() -. start);*)

	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify_assign nnconvex_constraint;
	
	(* The end *)
	()*)

(** Performs the intersection of a nnconvex_constraint with a linear_constraint; the first nnconvex_constraint is modified, the second is not *)
let nnconvex_intersection_assign nnconvex_constraint nnconvex_constraint' =
	(* Actual call to PPL *)
	ppl_Pointset_Powerset_NNC_Polyhedron_intersection_assign nnconvex_constraint nnconvex_constraint';
	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify_assign nnconvex_constraint;
	(* The end *)
	()


(** Performs the intersection of a nnconvex_constraint with a linear_constraint and return a new nnconvex_constraint (none of the arguments is modified) *)
let nnconvex_intersection nnconvex_constraint nnconvex_constraint' =
	(* First copy *)
	let copy = nnconvex_copy nnconvex_constraint in
	(* Perform intersection with side effects *)
	nnconvex_intersection_assign copy nnconvex_constraint';
	(* Return copy *)
	copy


(** Performs the intersection of a list of nnconvex_constraint and return a new nnconvex_constraint (none of the arguments is modified) *)
let nnconvex_intersection_list nnconvex_constraint_list =
	(* First create true constraint *)
	let nnconvex_constraint = true_nnconvex_constraint() in
	(* Perform intersection with side effects *)
	List.iter (fun nnconvex_constraint' -> nnconvex_intersection_assign nnconvex_constraint nnconvex_constraint') nnconvex_constraint_list;
	(* Return copy *)
	nnconvex_constraint



(** Performs the union of a nnconvex_constraint with a linear_constraint; the nnconvex_constraint is modified, the linear_constraint is not *)
let nnconvex_union_with_linear_constraint nnconvex_constraint linear_constraint =
	ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct nnconvex_constraint linear_constraint;
	simplify_assign nnconvex_constraint;
	(* The end *)
	()




(** Performs the union of a nnconvex_constraint with another nnconvex_constraint; the first nnconvex_constraint is modified, the second is not *)
let nnconvex_union nnconvex_constraint nnconvex_constraint' =
	(* Get the disjuncts of the second nnconvex_constraint *)
	let disjuncts = get_disjuncts nnconvex_constraint' in
	(* Add each of them as a union *)
	List.iter (nnconvex_union_with_linear_constraint nnconvex_constraint) disjuncts


(** Performs the difference between a first nnconvex_constraint and a second nnconvex_constraint; the first is modified, the second is not *)
let nnconvex_difference_assign nnconvex_constraint nnconvex_constraint' =
	ppl_Pointset_Powerset_NNC_Polyhedron_difference_assign nnconvex_constraint nnconvex_constraint';
	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify_assign nnconvex_constraint;
	(* The end *)
	()

(** Perform the non-convex difference of two constraints *)
let nnconvex_difference nnconvex_constraint nnconvex_constraint' =
	(* First copy the first *)
	let diff = nnconvex_copy nnconvex_constraint in
	(* Perform difference with side effects *)
	nnconvex_difference_assign diff nnconvex_constraint';
	(* Return diff *)
	diff


(** Negate a nnconvex_constraint *)
(*** NOTE: not c = true \ c ***)
let negate nnconvex_constraint =
	(* First generate true constraint *)
	let diff = true_nnconvex_constraint () in
	(* Perform difference with side effects *)
	nnconvex_difference_assign diff nnconvex_constraint;
	(* Return diff *)
	diff

(*-----------------------------------------------------------*)
(** Time elapsing and past *)
(*-----------------------------------------------------------*)

(** Apply time elapsing to an nnconvex_constraint with variable_elapse elapsing, and variable_constant remaining constant; version with side effects *)
(* 'reverse_direction' should be minus_one for growing, one for decreasing *)
let nnconvex_time_elapse_assign_gen reverse_direction variable_elapse variable_constant nnconvex_constraint =
	(* Create the inequalities var = 1, for var in variable_elapse *)
	let inequalities_elapse = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] reverse_direction in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variable_elapse in
	(* Create the inequalities var = 0, for var in variable_constant *)
	let inequalities_constant = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variable_constant in
	(* Convert both sets of inequalities to a nnconvex_constraint *)
	let nnconvex_constraint_time = nnconvex_constraint_of_linear_constraint (make (List.rev_append inequalities_elapse inequalities_constant)) in
	(* Assign the time elapsing using PPL *)
	ppl_Pointset_Powerset_NNC_Polyhedron_time_elapse_assign nnconvex_constraint nnconvex_constraint_time


(** Time elapsing function *)
let nnconvex_time_elapse_assign = nnconvex_time_elapse_assign_gen NumConst.minus_one


(** Apply time elapsing to an nnconvex_constraint with variable_elapse elapsing, and variable_constant remaining constant *)
let nnconvex_time_elapse variable_elapse variable_constant nnconvex_constraint =
	let nnconvex_constraint' = nnconvex_copy nnconvex_constraint in
	nnconvex_time_elapse_assign variable_elapse variable_constant nnconvex_constraint';
	nnconvex_constraint'



(** Time elapsing function, in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) *)
let nnconvex_time_past_assign variables_elapse variables_constant nnconvex_constraint =
	(* 1) Apply generic function *)
	nnconvex_time_elapse_assign_gen NumConst.one variables_elapse variables_constant nnconvex_constraint;
	
	(* 2) Constrain the elapsing variables to be non-negative! *)
	(* Create the inequalities var >= 0, for var in variables_elapse *)
	let inequalities_nonnegative = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_ge
	) variables_elapse in
	(* Take intersection *)
	nnconvex_intersection_assign nnconvex_constraint (nnconvex_constraint_of_linear_constraint (make inequalities_nonnegative))


(** Apply time elapsing in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) to an nnconvex_constraint with variable_elapse elapsing, and variable_constant remaining constant *)
let nnconvex_time_past variable_elapse variable_constant nnconvex_constraint =
	let nnconvex_constraint' = nnconvex_copy nnconvex_constraint in
	nnconvex_time_past_assign variable_elapse variable_constant nnconvex_constraint';
	nnconvex_constraint'






(** Create a new nnconvex_constraint from a list of linear_constraint *)
let nnconvex_constraint_of_linear_constraints (linear_constraints : linear_constraint list) =
	(* Create a false constraint *)
	let result = false_nnconvex_constraint() in
	(* Add each constraint as a disjunction *)
	List.iter (fun linear_constraint -> 
		nnconvex_union_with_linear_constraint result linear_constraint;
	) linear_constraints;
	(* Return result *)
	result



let adhoc_nnconvex_hide variables nnconvex_constraint =
	(* 1) Get disjuncts *)
	let disjuncts = get_disjuncts nnconvex_constraint in
	
	(* 2) Hide in each disjuncts *)
	let disjuncts_hidden = List.map (hide variables) disjuncts in
	
	(* 3) Recreate the nnconvex_constraint *)
	nnconvex_constraint_of_linear_constraints disjuncts_hidden
	


let nnconvex_hide = adhoc_nnconvex_hide


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Operations without modification} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Exhibit a point in a nnconvex_constraint; raise EmptyConstraint if the constraint is empty. *)
(*** NOTE: we try to exhibit in each dimension the minimum, except if no minimum (infimum) in which case we get either the middle between the infimum and the supremum (if any supremum), or the infimum if no supremum; and dually if no infimum. ***)
let nnconvex_constraint_exhibit_point nnconvex_constraint =
	(* First quick check that the constraint is satisfiable *)
	if nnconvex_constraint_is_false nnconvex_constraint then raise EmptyConstraint;
	
	(* Create an array for storing the valuation *)
	let valuations = Array.make !total_dim NumConst.zero in
	
	(* Print some information *)
	print_message Verbose_high "Entering nnconvex_constraint_exhibit_point…";
	
	(* Print some information *)
	print_message Verbose_high "Copying the constraint…";
	
	(* Copy the constraint, as we will restrict it dimension by dimension *)
	let restricted_nnconvex_constraint = nnconvex_copy nnconvex_constraint in
	
	(* Iterate on dimensions *)
	for dimension = 0 to !total_dim - 1 do
	
		(* Find the valuation for this dimension *)
		let valuation =

		(* If variable unbound: arbitrarily return 1 *)
		if not (ppl_Pointset_Powerset_NNC_Polyhedron_constrains restricted_nnconvex_constraint dimension) then(
			
			(* Print some information *)
			print_message Verbose_high ("Dimension " ^ (string_of_int dimension) ^ " is unconstrained here.");
				
			(* return 1 *)
			NumConst.one
		)
		else(
			
			(* Print some information *)
			print_message Verbose_high ("Getting infimum of dimension " ^ (string_of_int dimension) ^ "…");
		
			(* Get infimum *)
		
		(* Create linear expression with just the dimension of interest *)
		let linear_expression : Ppl.linear_expression = ppl_linear_expression_of_linear_term (make_linear_term [(NumConst.one, dimension)] NumConst.zero) in
		
			(*** DOC: function signature is val ppl_Pointset_Powerset_NNC_Polyhedron_minimize : pointset_powerset_nnc_polyhedron ->
		linear_expression -> bool * Gmp.Z.t * Gmp.Z.t * bool ***)
			let bounded_from_below, infimum_numerator, infimum_denominator, is_minimum = ppl_Pointset_Powerset_NNC_Polyhedron_minimize restricted_nnconvex_constraint linear_expression in
			
			(* Build the infimum *)
			let infimum = NumConst.numconst_of_zfrac infimum_numerator infimum_denominator in

			(* Print some information *)
			if verbose_mode_greater Verbose_high then
				print_message Verbose_high ("Infimum of dimension " ^ (string_of_int dimension) ^ " is " ^ (NumConst.string_of_numconst infimum) ^ ". Is it a minimum? " ^ (string_of_bool is_minimum));
		
			(* If minimum: pick it *)
			if bounded_from_below && is_minimum then(
				(* Return the infimum *)
				infimum
				
			)else(
			(* Otherwise find supremum *)
				let bounded_from_above, supremum_numerator, supremum_denominator, is_maximum = ppl_Pointset_Powerset_NNC_Polyhedron_maximize restricted_nnconvex_constraint linear_expression in
				
				(* Build the supremum *)
				let supremum = NumConst.numconst_of_zfrac supremum_numerator supremum_denominator in
				
				(* Print some information *)
				if verbose_mode_greater Verbose_high then
					print_message Verbose_high ("Supremum of dimension " ^ (string_of_int dimension) ^ " is " ^ (NumConst.string_of_numconst supremum) ^ ". Is it a maximum? " ^ (string_of_bool is_maximum));
					
				(* Case 0: bounded from neither below nor above: return 1 (arbitrarily) *)
				if not bounded_from_below && not bounded_from_above then(
					(* Print some information *)
					print_message Verbose_high ("Dimension " ^ (string_of_int dimension) ^ " is bounded from neither below nor above: pick 1");
					
					(* Return 1 *)
					NumConst.one
				)

				(* Case 1: infimum and no supremum: return infimum + 1 *)
				else if bounded_from_below && not bounded_from_above then
					NumConst.add infimum NumConst.one
				
				(* Case 2: no infimum and supremum: return 1 if 1 is allowed, otherwise supremum - 1, i.e., min(1, supremum - 1) *)
				else if not bounded_from_below && bounded_from_above then
					NumConst.min NumConst.one (NumConst.sub supremum NumConst.one)
				
				(* Case 3: infimum and supremum: return (infimum + supremum) / 2 *)
				else(
					(* If empty constraint: problem, raise exception *)
					if NumConst.l supremum infimum || (NumConst.le supremum infimum && (not is_maximum || not is_minimum)) then raise (InternalError "This situation is not supposed to happen, as the constraint was shown to be non-empty");
					
					(* Compute average  *)
					NumConst.div (
						NumConst.add infimum supremum
					) (NumConst.numconst_of_int 2)
				)
			) (* end else if no minimum *)
		) (* end else if not unbound *)
		in

		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			print_message Verbose_medium ("Valuation found for dimension " ^ (string_of_int dimension) ^ ": " ^ (NumConst.string_of_numconst valuation) ^ "");
		);
	
		(* Store it *)
		valuations.(dimension) <- valuation;
			
		(* Constrain the constraint with the found valuation, i.e., dimension = valuation *)
		let valuation_assignment = nnconvex_constraint_of_linear_constraint (make [
			make_linear_inequality
				(* "dimension - valuation = 0" *)
				(make_linear_term [(NumConst.one, dimension)] (NumConst.neg valuation))
				Op_eq
			]) in
		nnconvex_intersection_assign restricted_nnconvex_constraint valuation_assignment;
	
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high ("Current constraint after handling dimension " ^ (string_of_int dimension) ^ " is: " ^ (string_of_nnconvex_constraint (fun v -> "v_" ^ (string_of_int v)) restricted_nnconvex_constraint ) ^ "");
		);
		
	done;
	
	(* Return functional view *)
	(fun variable -> valuations.(variable))

(** Given two zones z1 and z2, such that z2 is the successor of z1, and given z a subset of z2, then nnconvex_constraint_zone_predecessor z1 z2 z t nott r computes the zone predecessor of z within z1, given the set t (nott) of variables sensitive (resp. insensitive) to time-elapsing, and r the variables reset between z1 and z2. *)
(*** NOTE: no check is made that z2 is a successor of z1, nor that z is a subset of z2 ***)
let nnconvex_constraint_zone_predecessor z1 z2 z variable_elapse variable_constant variable_reset =
	(* Copy z, to avoid side-effects *)
	let nnconvex_constraint = nnconvex_copy z in
	
	(* Compute time-past of z *)
	
	
	raise (InternalError "not implemented")


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to a list of linear_constraint} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** Converts a nnconvex_constraint into a list of linear_constraint such that the union of this list is equal to the nnconvex_constraint *)
let linear_constraint_list_of_nnconvex_constraint =
	(* Get the disjuncts *)
	get_disjuncts


