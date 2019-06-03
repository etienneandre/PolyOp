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
 * Last modified: 2019/06/03
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

{
open InputParser

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)
	| "--" [^'\n']* '\n' { line := !line + 1 ; token lexbuf }     (* skip Hytech-style comments *)

	(* C style comments *)
	| "/*"
		{ comment_depth := 1;
		comment_c lexbuf;
		token lexbuf }
	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

	| "and"            { CT_AND }
	| "diff"           { CT_DIFF }
	| "elapsing"       { CT_ELAPSING }
	| "equal"          { CT_EQUAL }
	| "exhibitpoint"   { CT_EXHIBIT_POINT }
	| "false"          { CT_FALSE }
	| "False"          { CT_FALSE }
	| "hide"           { CT_HIDE }
	| "in"             { CT_IN }
	| "included"       { CT_INCLUDED }
	| "not"            { CT_NOT }
	| "nothing"        { CT_NOTHING }
	| "or"             { CT_OR }
	| "OR"             { CT_OR }
	| "||"             { CT_OR }
	| "past"           { CT_PAST }
	| "satisfiable"    { CT_SATISFIABLE }
	| "simplify"       { CT_SIMPLIFY }
	| "true"           { CT_TRUE }
	| "True"           { CT_TRUE }

	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm } 
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }
	| '"' [^'"']* '"' as lxm { STRING lxm } (* a string between double quotes *)

	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| '>'              { OP_G }
	| ":="             { OP_ASSIGN }

	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '*'              { OP_MUL }
	| '/'              { OP_DIV }

	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

	| '&'              { AMPERSAND }
	| "&&"             { DOUBLEAMPERSAND }
	| ','              { COMMA }
	| '\''             { APOSTROPHE }
(* 	| '|'              { PIPE } *)
(* 	| "||"             { DOUBLEPIPE } *)
	| ':'              { COLON }
	| ';'              { SEMICOLON }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}



(* C style comments *)
and comment_c = parse
    "/*"  { incr comment_depth; comment_c lexbuf }
  | "*/"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_c lexbuf }
  | eof
    { failwith "End of file inside a comment." }
	
  | '\n'  { line := !line + 1 ; comment_c lexbuf }
  | _     { comment_c lexbuf }
  
(* OCaml style comments *)
and comment_ocaml = parse
    "(*"  { incr comment_depth; comment_ocaml lexbuf }
  | "*)"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
