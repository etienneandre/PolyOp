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


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string
exception Found

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)


(****************************************************************)
(** Useful functions on string *)
(****************************************************************)
(* Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""

(* Returns a fresh string made of 'n' times 's' *)
let string_n_times n s =
	string_of_array_of_string (Array.make n s)

(* 'add_digits n i' adds (m-n) '0' in front of 'i', if 'i' is an integer with only 'm' digits; result is always a string *)
let add_digits n i =
	(* Convert to string *)
	let str_i = string_of_int i in
	(* Count the number of digits *)
	let size_i = String.length str_i in
	(
		(* Add more *)
		if size_i <= n then
			(string_n_times (n - size_i) "0")
		(* Otherwise keep unchanged *)
		else ""
	) ^ str_i

(* Round a float with 3 digits after comma, and convert to string *)
let round3_float d =
(* 	((float_of_int) (int_of_float (d *. 1000.0))) /. 1000.0 *)
	(* Integer part *)
	let int_part = string_of_int (int_of_float (floor d)) in
	(* Floating part on 3 digits *)
	let real_part = add_digits 3 ((int_of_float (d *. 1000.0)) mod 1000) in
	(* Concatenate both *)
	int_part ^ "." ^ real_part



(****************************************************************)
(** Version string *)
(****************************************************************)

let version = "1.0 alpha"

let version_string () = 
	("PolyOp " ^ version)



let print_header_string () =

	let header_string = 
	
	(* Build info *)
	let build_info = "Build: " ^ BuildInfo.build_number ^ " (" ^ BuildInfo.build_time ^ ")" in
	let length_header = 54 in
	
	let program_name = version_string () in
	
	"************************************************************\n"
	^ "*  " ^ program_name ^ (string_n_times (length_header - (String.length program_name)) " ") ^ "  *\n"
	^ "*                                                          *\n"
	^ "*          Interface to the Parma Polyhedra Library (PPL)  *\n"
	^ "*                                           Étienne André  *\n"
	^ "*                                             2011 - " ^ (BuildInfo.build_year) ^ "  *\n"
	^ "*                        National University of Singapore  *\n"
	^ "*                   IRCCyN, École Centrale Nantes, France  *\n"
	^ "*                 Université Paris 13, LIPN, CNRS, France  *\n"
(* 	^ "*  " ^ (string_n_times (length_header - (String.length imitator_url)) " ") ^ imitator_url ^ "  *\n" *)
	^ "*                                                          *\n"
	^ "*  " ^ (string_n_times (length_header - (String.length build_info)) " ") ^ build_info ^ "  *\n"
	^ "************************************************************\n"
    ^ "This program comes with ABSOLUTELY NO WARRANTY.\n"
    ^ "This is free software, and you are welcome to redistribute it under certain conditions.\n\n"
	
	in print_string header_string



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

(* Associate an integer to each verbose mode *)
let level_of_verbose = function
	| Verbose_nodebug -> 0
	| Verbose_standard -> 1
	| Verbose_low -> 2
	| Verbose_medium -> 3
	| Verbose_high -> 4
	| Verbose_total -> 5

(* The global verbose mode *)
type global_verbose_mode_type =
	| Verbose_mode_not_set
	| Verbose_mode_set of verbose_mode

(* set to nodebug by default *)
let global_verbose_mode = ref (Verbose_mode_set Verbose_nodebug)

let timed_mode = ref false


(* Return true if the global verbose mode is greater than 'verbose_mode', false otherwise *)
let verbose_mode_greater verbose_mode =
	(* Get the global verbose mode *)
	let global_verbose_mode =
	match !global_verbose_mode with
	| Verbose_mode_not_set -> raise (InternalError ("The verbose mode has not been set, impossible to access it."))
	| Verbose_mode_set global_verbose_mode -> global_verbose_mode
	in
	(* Compare *)
	(level_of_verbose global_verbose_mode) >= (level_of_verbose verbose_mode)


(* Convert a string into a verbose_mode; raise Not_found if not found *)
let verbose_mode_of_string verbose_mode =
	if verbose_mode = "nodebug" then Verbose_nodebug
	else if verbose_mode = "standard" then Verbose_standard
	else if verbose_mode = "low" then Verbose_low
	else if verbose_mode = "medium" then Verbose_medium
	else if verbose_mode = "high" then Verbose_high
	else if verbose_mode = "total" then Verbose_total
	else raise Not_found

(* Set the verbose mode *)
let set_verbose_mode verbose_mode =
	(*match !global_verbose_mode with
	| Verbose_mode_not_set -> global_verbose_mode := Verbose_mode_set verbose_mode
	| Verbose_mode_set verbose_mode -> raise (InternalError ("The verbose mode has already been set, impossible to set it again."))*)
	global_verbose_mode := Verbose_mode_set verbose_mode

(* Get the verbose mode *)
let get_verbose_mode () =
	match !global_verbose_mode with
	| Verbose_mode_not_set -> raise (InternalError ("The verbose mode has not yet been set."))
	| Verbose_mode_set verbose_mode -> verbose_mode



(****************************************************************)
(** Global time counter *)
(****************************************************************)
let counter = ref (Unix.gettimeofday())

(* Compute a duration in ms *)
let duration_of_float d =
	((float_of_int) (int_of_float (d *. 1000.0))) /. 1000.0

(** Get the value of the counter *)
let get_time() =
	(Unix.gettimeofday()) -. (!counter)

(* Compute the duration since time t *)
let time_from t =
	(Unix.gettimeofday()) -. t

(* Print a number of seconds *)
let string_of_seconds nb_seconds =
	let duration = round3_float nb_seconds in
	let plural = (if nb_seconds <= 1.0 then "" else "s") in
	duration ^ " second" ^ plural


(* Create a string of the form 'after x seconds', where x is the time since the program started *)
let after_seconds () =
	"after " ^ (string_of_seconds (get_time()))

(** Set the timed mode *)
let set_timed_mode () =
	timed_mode := true


(****************************************************************)
(** Useful functions on lists *)
(****************************************************************)

(* Check if a list is empty *)
let list_empty = function
	| [] -> true
	| _ -> false
	
(* Return a random element in a list *)
let random_element l =
	Random.self_init();
	let nth = Random.int (List.length l) in
	List.nth l nth

(** list_of_interval l u Create a fresh new list filled with elements [l, l+1, ..., u-1, u] *)
let rec list_of_interval l u =
	if ( l > u )
		then []
	else l :: (list_of_interval ( l + 1 ) u)


(* Intersection of 2 lists (keeps the order of the elements as in l1) *)
let list_inter l1 l2 =
	List.filter (fun e -> List.mem e l2) l1

(* Difference of 2 lists l1 \ l2 (keeps the order of the elements as in l1) *)
let list_diff l1 l2 =
	List.filter (fun e -> not (List.mem e l2)) l1

(* Union of 2 lists: add elements of l2 in tail of l1; order is preserved *)
let list_union l1 l2 =
	List.rev (List.fold_left
		(fun l e -> if List.mem e l1 then l else e::l)
		(List.rev l1)
		l2
	)

(* Tail-recursive function for 'append' *)
let list_append l1 l2 =
	ExtList.(@) l1 l2
	

(* Return a list where every element only appears once *)
let list_only_once l =
	List.rev (List.fold_left
		(fun current_list e -> if List.mem e current_list then current_list else e::current_list)
		[]
		l
	)


(* Return a sublist of a list with only the elements existing several times *)
let elements_existing_several_times l =
	let rec elements_existing_several_times_rec elements = function
		| [] -> elements
		| first :: rest ->
			(* Take the elements equal to "first" away from "rest" *)
			let elements_equal_to_first, sub_rest = List.partition (fun a -> a = first) rest in
			match elements_equal_to_first with
			(* Empty list: keep searching *)
			| [] -> elements_existing_several_times_rec elements sub_rest
			(* Non-empty list: add "first" to the list of elements existing several times, and keep searching *)
			| _ -> elements_existing_several_times_rec (first :: elements) sub_rest
	in
	List.rev (elements_existing_several_times_rec [] l)


(****************************************************************)
(** Useful functions on arrays *)
(****************************************************************)

(* Check if an element belongs to an array *)
let in_array e a =
	List.mem e (Array.to_list a)

(* Returns the (first) index of an element in an array, or raise Not_found if not found *)
let index_of e a =
	let length = Array.length a in
	let found = ref false in
	let i = ref 0 in
	while not !found && !i < length do
		if a.(!i) = e then found := true;
		i := !i + 1;
	done;
	if not !found then raise Not_found;
	(* Return the index *)
	(!i - 1)
	
(* Return the list of the indexes whose value is true *)
let true_indexes a =
	let list_of_indexes = ref [] in
	Array.iteri (fun index value ->
		if value then (list_of_indexes := index :: !list_of_indexes);
	) a;
	List.rev (!list_of_indexes)


(* Shuffle an array *)
(* let shuffle_array = Array.sort (fun _ _ -> (Random.int 3) - 1) *)

(** exists p {a1; ...; an} checks if at least one element of the Array satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
let array_exists p a =
	try(
		Array.iter (fun 
			elem -> if p elem then (raise Found);
		) a;
		(* Not found *)
		false
	) with Found -> true


(****************************************************************)
(** Useful functions on dynamic arrays *)
(****************************************************************)

(* exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
let dynArray_exists p a =
	try(
		DynArray.iter (fun 
			elem -> if p elem then (raise Found);
		) a;
		(* Not found *)
		false
	) with Found -> true


(****************************************************************)
(** Useful functions on string *)
(****************************************************************)
(* Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""

(* Convert a list of string into a string *)
let string_of_list_of_string =
	List.fold_left (fun the_string s -> the_string ^ s) ""

(* Convert an array of string into a string with separators *)
let string_of_array_of_string_with_sep sep a =
	let length = Array.length a in
	if length = 0 then "" else(
		let the_string = ref "" in
		for i = 0 to length - 2 do
			the_string := (!the_string) ^ a.(i) ^ sep
		done;
		!the_string ^ a.(length - 1)
	)

(* Convert a list of string into a string with separators *)
let string_of_list_of_string_with_sep sep l =
	string_of_array_of_string_with_sep sep (Array.of_list l)


(* 's_of_int i' Return "s" if i > 1, "" otherwise *)
let s_of_int i =
	if i > 1 then "s" else ""

(****************************************************************)
(** Useful functions on booleans *)
(****************************************************************)
(* Evaluate both part of an 'and' comparison and return the conjunction *)
let evaluate_and a b =
(*	let computed_a = a in
	let computed_b = b in
	computed_a && computed_b*)
	a && b

(* Evaluate both part of an 'or' comparison and return the disjunction *)
let evaluate_or a b =
	a || b


(****************************************************************)
(** Messages *)
(****************************************************************)
(* Print a string *)
let print_message_generic message =
	(* Timed mode *)
	let time_info =
		if !timed_mode then (" (at t = " ^ (string_of_seconds (get_time())) ^ ")")
		else "" in
	(* Print message *)
	print_string (message ^ time_info ^ "\n");
	(* Flush! *)
	flush stdout


(* Print a message if global_verbose_mode >= message_verbose_mode *)
let print_message message_verbose_mode message =
	(* Only print the message if its message_verbose_mode is smaller or equal to the global_verbose_mode *)
	if verbose_mode_greater message_verbose_mode then
		(* Compute the verbose level *)
		let verbose_level = level_of_verbose message_verbose_mode in
		(* Find number of blanks for indentation *)
		let nb_spaces = if verbose_level-1 > 0 then verbose_level-1 else 0 in
		(* Create blanks proportionnally to the verbose_level (at least one space) *)
		let spaces = " " ^ string_of_array_of_string (Array.make nb_spaces "   ") in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
		(* Print *)
		print_message_generic formatted_message



(* Print a warning *)
let print_warning message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** Warning: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print *)
	print_message_generic formatted_message


(* Print an error *)
let print_error message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** ERROR: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print *)
	print_message_generic formatted_message


(****************************************************************)
(** Terminating functions *)
(****************************************************************)

(* Abort program *)
let abort_program () =
	print_error ("Program aborted (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush stdout;
	exit(0)

(* Terminate program *)
let terminate_program () =
	print_newline();
	print_message Verbose_standard ("PolyOp successfully terminated (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush stdout;
	exit(0)

