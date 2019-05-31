(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 *
 * Author:        Étienne André
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

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractStructure

(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram


(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the parsing structure into an abstract program *)
val abstract_program_of_parsing_structure : ParsingStructure.parsing_structure -> abstract_program
