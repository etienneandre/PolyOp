(************************************************************
 *
 *                     PolyOp
 *
 * Convert a parsing structure into an abstract program
 *
 * National University of Singapore
 *
 * Author:        Etienne ANDRE
 * Created:       2011/04/27
 * Last modified: 2011/05/30
 *
 ************************************************************)

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open ParsingStructure
open AbstractStructure
open ProgramPrinter


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram



(****************************************************************)
(** Constraint conversion *)
(****************************************************************)
exception False_exception


(*--------------------------------------------------*)
(* Retrieve a variable_index from a variable_name *)
(*--------------------------------------------------*)
let index_of_variable_name index_of_variables variable_name =
	try(
		(* Find the variable_index *)
		Hashtbl.find index_of_variables variable_name
	) with Not_found -> raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although it was checked before."))

(*--------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*--------------------------------------------------*)
let array_of_coef_of_linear_expression index_of_variables linear_expression =
	(* Create an array of coef *)
	let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in
	
	(* Internal function to update the array for a linear term *)
	let update_array_linear_term mul_coef = function
		(* Case constant -> update the constant with the coef *)
		| Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
		(* Case variables -> update the array with the coef  *)
		| Variable (coef, variable_name) -> let variable_index = index_of_variable_name index_of_variables variable_name in
			(* Update the variable with its coef *)
			array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
	in

	(* Internal function to update the array for a linear expression *)
	let rec update_array_linear_expression = function
		| Linear_term lt -> update_array_linear_term NumConst.one lt
		| Linear_plus_expression (le, lt) ->
			(* Fill the array with le *)
			update_array_linear_expression le;
			(* Fill the array with lt *)
			update_array_linear_term NumConst.one lt;
		| Linear_minus_expression (le, lt) ->
			(* Fill the array with le *)
			update_array_linear_expression le;
			(* Fill the array with lt *)
			update_array_linear_term NumConst.minus_one lt;
	in
	(* Call the recursive function *)
	update_array_linear_expression linear_expression;
	(* Return the array of coef and the constant *)
	array_of_coef, !constant


(*--------------------------------------------------*)
(* Convert an array of variable coef into a linear term *)
(*--------------------------------------------------*)
let linear_term_of_array (array_of_coef, constant) =
	(* Create an empty list of members *)
	let members = ref [] in
	(* Iterate on the coef *)
	Array.iteri (fun variable_index coef ->
		if NumConst.neq coef NumConst.zero then (
			(* Add the member *)
			members := (coef, variable_index) :: !members;
		);
	) array_of_coef;
	(* Create the linear term *)
	LinearConstraint.make_linear_term !members constant
	

(*--------------------------------------------------*)
(* Direct conversion of a ParsingStructure.linear_expression into a Linear_term.linear_term *)
(*--------------------------------------------------*)
let linear_term_of_linear_expression index_of_variables linear_expression =
	let array_of_coef, constant = array_of_coef_of_linear_expression index_of_variables linear_expression in
	linear_term_of_array (array_of_coef, constant)


(*--------------------------------------------------*)
(* Perform the substraction of 2 NumConst array of same size *)
(*--------------------------------------------------*)
let sub_array array1 array2 =
	(* Create the result *)
	let result = Array.make (Array.length array1) NumConst.zero in
	(* Iterate on both arrays *)
	for i = 0 to (Array.length array1) - 1 do
		(* Perform array1 - array2 *)
		result.(i) <- NumConst.sub array1.(i) array2.(i);
	done;
	(* Return the result *)
	result


(*--------------------------------------------------*)
(* Convert a ParsingStructure.linear_constraint into a Constraint.linear_inequality *)
(*--------------------------------------------------*)
let linear_inequality_of_linear_constraint index_of_variables (le1, relop, le2) =
	(* Get the array of variables and constant associated to the linear terms *)
	let array1, constant1 = array_of_coef_of_linear_expression index_of_variables le1 in
	let array2, constant2 = array_of_coef_of_linear_expression index_of_variables le2 in
	(* Consider the operator *)
	match relop with
	(* a < b <=> b - a > 0 *)
	| OP_L ->
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_g
(* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_g *)

	(* a <= b <=> b - a >= 0 *)
	| OP_LEQ ->
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_ge
(* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_ge *)

(* a = b <=> b - a = 0 *)
	| OP_EQ -> 
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_eq
	
(* 	(Constraint.substract_linear_terms lt1 lt2), Constraint.Op_eq *)

	(* a >= b <=> a - b >= 0 *)
	| OP_GEQ ->
		(* Create the array *)
		let array12 = sub_array array1 array2 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant1 constant2 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_ge
(* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_ge *)

	(* a > b <=> a - b > 0 *)
	| OP_G ->
		(* Create the array *)
		let array12 = sub_array array1 array2 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant1 constant2 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_g
(* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_g *)


(*--------------------------------------------------*)
(* Convert a ParsingStructure.convex_predicate into a Constraint.linear_constraint *)
(*--------------------------------------------------*)

let linear_constraint_of_convex_predicate index_of_variables convex_predicate =
	try(
	
	
(*	print_int (!i);
	print_newline();
	i := !i + 1;	
	if (!i > 100) then (raise False_exception);*)
	
	(* Compute a list of inequalities *)
	let linear_inequalities = List.fold_left
		(fun linear_inequalities linear_inequality -> 
		match linear_inequality with
		| True_constraint -> linear_inequalities
		| False_constraint -> raise False_exception
		| Linear_constraint (le1, relop, le2) -> (linear_inequality_of_linear_constraint index_of_variables (le1, relop, le2)) :: linear_inequalities
	) [] convex_predicate
	in LinearConstraint.make linear_inequalities
	(* Stop if any false constraint is found *)
	) with False_exception -> LinearConstraint.false_constraint ()


(****************************************************************)
(** Variables *)
(****************************************************************)
let get_variable_names_in_linear_term = function
	| Constant _ -> []
	| Variable (_, variable_name) -> [variable_name]


let rec get_variable_names_in_linear_expression = function
	| Linear_term lt -> get_variable_names_in_linear_term lt
	| Linear_plus_expression (le, lt) -> 
		List.rev_append
			(get_variable_names_in_linear_expression le)
			(get_variable_names_in_linear_term lt)
	| Linear_minus_expression (le, lt) -> 
		List.rev_append
			(get_variable_names_in_linear_expression le)
			(get_variable_names_in_linear_term lt)


let rec get_variable_names_in_linear_constraint = function
	| True_constraint -> []
	| False_constraint -> []
	| Linear_constraint (le1, _, le2) ->
		List.rev_append
			(get_variable_names_in_linear_expression le1)
			(get_variable_names_in_linear_expression le2)


let rec get_variable_names_in_convex_predicate = function
	| [] -> []
	| linear_constraint :: rest ->
		List.rev_append 
			(get_variable_names_in_linear_constraint linear_constraint)
			(get_variable_names_in_convex_predicate rest)


let rec get_variable_names_in_constraint = function
	| Parsop_and cp_list ->
		List.fold_left (fun a b -> List.rev_append a (get_variable_names_in_constraint b)) [] cp_list
	| Parsop_hide (vars, c) -> 
		List.rev_append
			vars
			(get_variable_names_in_constraint c)
	| Parsop_simplify c -> get_variable_names_in_constraint c
	| Parsop_time_elapsing (vars, c) -> 
		List.rev_append
			vars
			(get_variable_names_in_constraint c)
	| Parsop_convex cp -> get_variable_names_in_convex_predicate cp


let get_variable_names_in_bool = function
	| Parsop_equal (cp1, cp2) -> 
		List.rev_append
			(get_variable_names_in_constraint cp1)
			(get_variable_names_in_constraint cp2)
	| Parsop_included (cp1, cp2) -> 
		List.rev_append
			(get_variable_names_in_constraint cp1)
			(get_variable_names_in_constraint cp2)
	| Parsop_satisfiable c -> get_variable_names_in_constraint c


let get_variable_names = function
	| Parsop_bool b -> get_variable_names_in_bool b
	| Parsop_constraint c -> get_variable_names_in_constraint c
	| Parsop_nothing -> []


(****************************************************************)
(** Program conversion *)
(****************************************************************)


(*--------------------------------------------------*)
(* Convert the parsing structure into an abstract program *)
(*--------------------------------------------------*)
let abstract_program_of_parsing_structure parsop =
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug functions *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug print function for arrays *)
	let debug_print_array =
		Array.iteri (fun i e ->
			print_message Debug_high ((string_of_int i) ^ " -> " ^ e)
		)
	in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get names *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the variable names *)
	let list_of_variable_names = get_variable_names parsop in
	
	(* Remove double names *)
	let list_of_variable_names = list_only_once list_of_variable_names in
	
	(* Get the number *)
	let nb_variables = List.length list_of_variable_names in

	(* The array of variables names ; index -> variable name *)
	let variables = Array.of_list list_of_variable_names in
	(* A (constant) hash table 'variable name -> index' *)
	let index_of_variables = Hashtbl.create nb_variables in
	for i = 0 to nb_variables - 1 do
		Hashtbl.add index_of_variables variables.(i) i;
	done;

	let variable_names = fun i -> variables.(i) in
	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	if (debug_mode_greater Debug_low) then (
		(* Number *)
		print_message Debug_low (
			(string_of_int nb_variables) ^ " variable" ^ (s_of_int nb_variables) ^ "."
		);
		
		(* Array *)
		print_message Debug_low ("\n*** Array of variable names:");
		debug_print_array variables;
	);


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the LinearConstraint manager *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let nb_integer_variables = 0 in
	(* 'nb_variables' represent the total number of variables *)
	let nb_real_variables = nb_variables in
	LinearConstraint.set_manager nb_integer_variables nb_real_variables;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the operation *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let convert_var = List.map (index_of_variable_name index_of_variables) in
	
	let rec convert_constraint = function
		| Parsop_and list_of_cp -> Op_and (List.map convert_constraint list_of_cp)
		| Parsop_hide (variable_names, c) -> Op_hide (convert_var variable_names, convert_constraint c)
		| Parsop_simplify c -> Op_simplify (convert_constraint c)
		| Parsop_time_elapsing (variable_names, c) -> Op_time_elapsing (convert_var variable_names, convert_constraint c)
		| Parsop_convex cp -> Op_convex (LinearConstraint.nnconvex_constraint_of_linear_constraint (linear_constraint_of_convex_predicate index_of_variables cp))
	in
	let convert_bool = function
		| Parsop_equal (cp1, cp2) -> Op_equal (convert_constraint cp1, convert_constraint cp2)
		| Parsop_included (cp1, cp2) -> Op_included (convert_constraint cp1, convert_constraint cp2)
		| Parsop_satisfiable cp -> Op_satisfiable (convert_constraint cp)
	in
	
	let op = match parsop with
		| Parsop_bool b -> Op_bool (convert_bool b)
		| Parsop_constraint c -> Op_constraint (convert_constraint c)
		| Parsop_nothing -> Op_nothing
	in


	(* Make the structure *)
	{
		(* Cardinality *)
		nb_variables = nb_variables;
		
		(* Names of the variable *)
		variable_names = variable_names;

		(* The operation to perform *)
		operation = op;
	}

