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
 * Last modified: 2016/02/29
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
(** Version string *)
(****************************************************************)

(* val version_string: string *)
val version_string: unit -> string

val print_header_string: unit -> unit


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)



(****************************************************************)
(** Verbose modes *)
(****************************************************************)

type verbose_mode =
	| Verbose_nodebug
	| Verbose_standard
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total


(* Return true if the global verbose mode is greater than 'verbose_mode', false otherwise *)
val verbose_mode_greater : verbose_mode -> bool

(* Convert a string into a verbose_mode; raise Not_found if not found *)
val verbose_mode_of_string : string -> verbose_mode

(* Set the verbose mode *)
val set_verbose_mode : verbose_mode -> unit

(* Get the verbose mode *)
val get_verbose_mode : unit -> verbose_mode



(****************************************************************)
(** Global time counter *)
(****************************************************************)

(* Compute the duration in ms between 2 times *)
(* val duration : float -> float -> float *)

(** Get the value of the counter *)
val get_time : unit -> float

(** Compute the duration since time t *)
val time_from : float -> float

(** Print a number of seconds *)
val string_of_seconds : float -> string

(** Create a string of the form 'after x seconds', where x is the time since the program started *)
val after_seconds : unit -> string

(** Set the timed mode *)
val set_timed_mode : unit -> unit


(****************************************************************)
(** Useful functions on lists *)
(****************************************************************)
(** Check if a list is empty *)
val list_empty : 'a list -> bool

(** Return a random element in a list *)
val random_element : 'a list -> 'a

(** list_of_interval a b Create a fresh new list filled with elements [a, a+1, ..., b-1, b] *)
val list_of_interval : int -> int -> int list

(** Intersection of 2 lists (keeps the order of the elements as in l1) *)
val list_inter : 'a list -> 'a list -> 'a list

(** Difference of 2 lists (keeps the order of the elements as in l1) *)
val list_diff : 'a list -> 'a list -> 'a list

(** Union of 2 lists *)
val list_union : 'a list -> 'a list -> 'a list

(** Tail-recursive function for 'append' *)
val list_append : 'a list -> 'a list -> 'a list

(** Return a list where every element only appears once *)
val list_only_once : 'a list -> 'a list

(** Filter the elements appearing several times in the list *)
val elements_existing_several_times : 'a list -> 'a list


(****************************************************************)
(** Useful functions on arrays *)
(****************************************************************)

(* Check if an element belongs to an array *)
val in_array : 'a -> 'a array -> bool

(* Returns the (first) index of an element in an array, or raise Not_found if not found *)
val index_of : 'a -> 'a array -> int

(* Return the list of the indexes whose value is true *)
val true_indexes : bool array -> int list

(* Shuffle an array *)
(* val shuffle_array : 'a array -> unit *)

(** exists p {a1; ...; an} checks if at least one element of the Array satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val array_exists : ('a -> bool) -> 'a array -> bool


(****************************************************************)
(** Useful functions on dynamic arrays *)
(****************************************************************)

(* exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val dynArray_exists : ('a -> bool) -> 'a DynArray.t -> bool


(****************************************************************)
(** Useful functions on string *)
(****************************************************************)
(* Convert an array of string into a string *)
val string_of_array_of_string : string array -> string

(* Convert a list of string into a string *)
val string_of_list_of_string : string list -> string

(* Convert an array of string into a string with separators *)
val string_of_array_of_string_with_sep : string -> string array -> string

(* Convert a list of string into a string with separators *)
val string_of_list_of_string_with_sep : string -> string list -> string

(* 's_of_int i' Return "s" if i > 1, "" otherwise *)
val s_of_int : int -> string

(****************************************************************)
(** Useful functions on booleans *)
(****************************************************************)
(* Evaluate both part of an 'and' comparison and return the conjunction *)
val evaluate_and : bool -> bool -> bool

(* Evaluate both part of an 'or' comparison and return the disjunction *)
val evaluate_or : bool -> bool -> bool


(****************************************************************)
(** Messages *)
(****************************************************************)
(*(* Print a message if global_verbose_mode >= message_verbose_mode *)
val print_verbose_message : verbose_mode -> verbose_mode -> string -> unit*)

(* Print a message if global_verbose_mode >= message_verbose_mode *)
val print_message : verbose_mode -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(* Print an error *)
val print_error : string -> unit


(****************************************************************)
(** Terminating functions *)
(****************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
