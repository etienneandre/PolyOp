(************************************************************
 *
 *                     PolyOp
 *
 * National University of Singapore
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Author:        Étienne André
 * Created:       2011/04/27
 * Last modified: 2024/09/17
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
(**************************************************)
(* GMP multi-precision integers *)
(**************************************************)
(**************************************************)

(**************************************************)
(* Type definition *)
(**************************************************)
type gmpz = Gmp.Z.t

(**************************************************)
(** {2 Constants} *)
(**************************************************)

(* 0, 1, -1 *)
val gmpz_zero      : gmpz
val gmpz_one       : gmpz
val gmpz_minus_one : gmpz

(**************************************************)
(** {2 Conversion functions} *)
(**************************************************)

(* Convert an integer to a Gmp.Z *)
val gmpz_of_int : int -> gmpz

(* Convert a Gmp.Z to a string *)
val string_of_gmpz : gmpz -> string

(**************************************************)
(** {2 Arithmetic functions} *)
(**************************************************)

(* Negation *)
val gmpz_neg : gmpz -> gmpz

(* Compute ceiling division *)
val gmpz_cdiv : gmpz -> gmpz -> gmpz

(* Compute floor division *)
val gmpz_fdiv : gmpz -> gmpz -> gmpz

(* Absolute *)
val gmpz_abs : gmpz -> gmpz


(**************************************************)
(** {2 Comparison functions} *)
(**************************************************)

(* Equal *)
val gmpz_equal : gmpz -> gmpz -> bool

(* Not equal *)
val gmpz_neq : gmpz -> gmpz -> bool

val gmpz_is_one : gmpz -> bool



(**************************************************)
(**************************************************)
(* GMP multi-precision rationals *)
(**************************************************)
(**************************************************)

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

val min : t -> t -> t
val max : t -> t -> t


(**************************************************)
(** {2 Comparison Functions} *)
(**************************************************)
val equal : t -> t -> bool
val neq : t -> t -> bool
val l : t -> t -> bool
val le : t -> t -> bool
val ge : t -> t -> bool
val g : t -> t -> bool
