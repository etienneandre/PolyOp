/************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * École Centrale Nantes, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Author:        Étienne André
 * Created:       2011/04/27
 * Last modified: 2024/01/09
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
************************************************************/

%{
open ParsingStructure;;
open Global;;
open NumConst;;
  
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA SEMICOLON

%token
	CT_AND
	CT_DIFF
	CT_ELAPSING CT_EQUAL CT_EXHIBIT_POINT
	CT_FALSE
	CT_HIDE
	CT_IN CT_INCLUDED
	CT_NOT CT_NOTHING
	CT_ONTO CT_OR
	CT_PAST CT_PROJECT
	CT_SATISFIABLE CT_SIMPLIFY
	CT_TRUE
	CT_UNION CT_UPDATE
	CT_ZONEPRED CT_ZONEPREDGR

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left DOUBLEPIPE PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%nonassoc CT_NOT        /* highest precedence */



%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	| oplist EOF { $1 }
;

/***********************************************
  MAIN DEFINITIONS
***********************************************/

oplist:
	| op_nonempty_oplist { $1 }
	| { [] }
;

op_nonempty_oplist:
	| op semicolon_opt op_nonempty_oplist { $1 :: $3 }
	| op semicolon_opt { [$1] }
;

op:
	| opbool { Parsop_bool $1 }
	| opconstraint { Parsop_constraint $1 }
	| oppoint { Parsop_point $1 }
	| CT_NOTHING { Parsop_nothing }
;

semicolon_opt:
	| SEMICOLON { }
	| { }
;

opbool:
	| CT_EQUAL opconstraint COMMA opconstraint { Parsop_equal ($2, $4) }
	| CT_EQUAL LPAREN opconstraint COMMA opconstraint RPAREN { Parsop_equal ($3, $5) }
	| CT_INCLUDED opconstraint CT_IN opconstraint { Parsop_included ($2, $4) }
/* 	| CT_INCLUDED LPAREN opconstraint CT_IN opconstraint RPAREN { Parsop_included ($3, $5) } */
	| CT_SATISFIABLE opconstraint { Parsop_satisfiable $2 }
/* 	| CT_SATISFIABLE LPAREN opconstraint RPAREN { Parsop_satisfiable $3 } // creates conflict, resolved automatically */
;

opconstraint:
	| CT_AND convex_predicate_list_with_par { Parsop_and $2 }
	| CT_DIFF opconstraint opconstraint { Parsop_diff ($2, $3) }
	| CT_DIFF opconstraint COMMA opconstraint { Parsop_diff ($2, $4) } /* NOTE: allowed for sake of convenience */
	| CT_ELAPSING variable_list_with_par_opt CT_IN opconstraint { Parsop_time_elapsing ($2, $4) }
	| CT_PAST variable_list_with_par_opt CT_IN opconstraint { Parsop_time_past ($2, $4) }
	| CT_HIDE variable_list_with_par_opt CT_IN opconstraint { Parsop_hide ($2, $4) }
	| CT_PROJECT opconstraint CT_ONTO variable_list_with_par_opt { Parsop_project ($4, $2) }
	| CT_NOT opconstraint { Parsop_not $2 }
	| CT_SIMPLIFY opconstraint { Parsop_simplify $2 }
	| CT_UNION convex_predicate_list_with_par { Parsop_union $2 }
	| CT_UPDATE updates CT_IN opconstraint { Parsop_update ($2, $4) }
	| CT_ZONEPRED opconstraint opconstraint opconstraint variable_list_with_par_opt_or_empty variable_list_with_par_opt_or_empty { Parsop_zonepred ($2 , $3, $4, $5, $6) }
	| LPAREN opconstraint RPAREN { $2 }
	/* zonepredgr(Zn-1, gn-1, Un-1, Zn, t, nont, gn, Un, Zn+1) */
	| CT_ZONEPREDGR opconstraint opconstraint updates opconstraint variable_list_with_par_opt_or_empty opconstraint updates opconstraint { Parsop_zonepredgr ($2 , $3, $4, $5, $6, $7, $8, $9) }
	| LPAREN opconstraint RPAREN { $2 }
	| nnconvex_predicate { Parsop_convex $1 }
;

oppoint:
	| CT_EXHIBIT_POINT opconstraint { Parsop_exhibit $2 }
;

/**********************************************/

/*** WARNING: many conflicts around here! ***/

variable_list_with_par_opt_or_empty:
	| LPAREN non_empty_variable_list RPAREN { $2 }
	| LPAREN RPAREN { [] }
	| non_empty_variable_list { $1 }
;

non_empty_variable_list:
	| NAME COMMA non_empty_variable_list { $1 :: $3 }
	| NAME { [$1] }
;


/**********************************************/

updates:
	| LPAREN updates_without_par RPAREN { $2 }
	| LPAREN RPAREN { [] }
;

updates_without_par:
	| update COMMA updates_without_par { $1 :: $3 }
	| update { [$1] }
;

update:
	| LPAREN update RPAREN { $2 }
	| NAME OP_ASSIGN linear_expression { $1 , $3 }
;


/**********************************************/
variable_list_with_par_opt:
	| variable_list { $1 }
	| LPAREN variable_list RPAREN { $2 }
;

variable_list:
	| NAME COMMA variable_list { $1 :: $3 }
	| NAME { [$1] }
	| { [] }
;


/**********************************************/
convex_predicate_list_with_par:
/* 	| convex_predicate_list { $1 } */
	| LPAREN convex_predicate_list RPAREN { $2 }
;

convex_predicate_list:
	| opconstraint COMMA convex_predicate_list { $1 :: $3 }
	| opconstraint { [$1] }
	| { [] }
;


/***********************************************
  RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES
***********************************************/

nnconvex_predicate:
	| convex_predicate CT_OR nnconvex_predicate {$1 :: $3}
	| convex_predicate {[$1]}
/* 	| { [] } */
;

convex_predicate:
	| linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
 	| LPAREN convex_predicate RPAREN { $2 }
;

linear_constraint:
	| linear_expression relop linear_expression { Linear_constraint ($1, $2, $3) }
	| CT_TRUE { True_constraint }
	| CT_FALSE { False_constraint }
;

relop:
	  OP_L { OP_L }
	| OP_LEQ { OP_LEQ }
	| OP_EQ { OP_EQ }
	| OP_GEQ { OP_GEQ }
	| OP_G { OP_G }
;

linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| NAME { Variable (NumConst.one, $1) }
/* 	| LPAREN linear_expression RPAREN { $2 } */
	| LPAREN linear_term RPAREN { $2 }
;

rational:
	integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	INT { $1 }
;

float:
  pos_float { $1 }
	| OP_MINUS pos_float { NumConst.neg $2 }  
;

pos_float:
  FLOAT { 
		let fstr = $1 in
		let point = String.index fstr '.' in
		(* get integer part *)
		let f = if point = 0 then ref NumConst.zero else (
			let istr = String.sub fstr 0 point in
		  ref (NumConst.numconst_of_int (int_of_string istr))
		) in		
		(* add decimal fraction part *)
		let numconst_of_char = function
			| '0' -> NumConst.zero
			| '1' -> NumConst.one
			| '2' -> NumConst.numconst_of_int 2
			| '3' -> NumConst.numconst_of_int 3
			| '4' -> NumConst.numconst_of_int 4
			| '5' -> NumConst.numconst_of_int 5
			| '6' -> NumConst.numconst_of_int 6
			| '7' -> NumConst.numconst_of_int 7
			| '8' -> NumConst.numconst_of_int 8
			| '9' -> NumConst.numconst_of_int 9
			| _ ->  raise (ParsingError (0,0)) in
		let ten = NumConst.numconst_of_int 10 in
		let dec = ref (NumConst.numconst_of_frac 1 10) in
		for i = point+1 to (String.length fstr) - 1 do
			let c = fstr.[i] in
			let d = numconst_of_char c in
			f := NumConst.add !f (NumConst.mul !dec d);
			dec := NumConst.div !dec ten 
		done;		
		!f
	} 
;

