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
 
(**************************************************)
(* Type definition *)
(**************************************************)

type t


(**************************************************)
(* Functions *)
(**************************************************)

(**************************************************)
(** {2 Constants} *)
(**************************************************)

val zero : t
val one : t
val minus_one : t


(**************************************************)
(** {2 User Conversions} *)
(**************************************************)

val numconst_of_string : string -> t
val numconst_of_int : int -> t
val numconst_of_float : float -> t
val numconst_of_frac : int -> int -> t
(* From num and den *)
val numconst_of_zfrac : Gmp.Z.t -> Gmp.Z.t -> t
val numconst_of_mpq : Gmp.Q.t -> t
val numconst_of_mpz : Gmp.Z.t -> t

val mpq_of_numconst : t -> Gmp.Q.t
val string_of_numconst : t -> string

val get_num : t -> Gmp.Z.t
val get_den : t -> Gmp.Z.t

(**************************************************)
(** {2 Arithmetic Functions} *)
(**************************************************)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val neg : t -> t
val abs : t -> t

(**************************************************)
(** {2 Comparison Functions} *)
(**************************************************)
val equal : t -> t -> bool
val neq : t -> t -> bool
val l : t -> t -> bool
val le : t -> t -> bool
val ge : t -> t -> bool
val g : t -> t -> bool
