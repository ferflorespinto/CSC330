(*
	Name: Jorge Fernando Flores Pinto
	ID: V00880059
	CSC330 - Fall 2018
	Assignment 3
*)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:
The function g takes two functions and a pattern.
Depending on the pattern, g performs certain operations with the functions
passed as arguments. f1 takes a unit, and f2 takes a string.
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(*
only_capitals = fn : string list -> string list
Takes a string list and returns a string list that has only the strings in the
argument that start with an uppercase letter.
*)
fun only_capitals(l:string list):string list =
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) l;

(*
longest_string1 = fn : string list -> string
Takes a string list and returns the longest string in the list. If the list is
empty, return "". In the case of a tie, return the string closest to the
beginning of the list.
*)
fun longest_string1(l:string list):string =
	let
		val f = fn (x, y) => if (String.size(x) > String.size(y))
						 	 then x
						 	 else y
	in
		List.foldl(f) "" l
	end;
(*
longest_string2 = fn : string list -> string
Exactly like longest_string1 except in the case of ties it returns the string
closest to the end of the list.
*)
fun longest_string2(l:string list):string =
	let
		val f = fn (x, y) => if (String.size(x) >= String.size(y))
							 then x
							 else y
	in
		List.foldl(f) "" l
	end;

(*
val longest_string_helper = fn
  : (int * int -> bool) * string * string list -> string

fun longest_string_helper (f, acc, sl) =
	case sl of
		[] => acc
	  | x::sl' => longest_string_helper(f,
		  		if f(String.size(x), String.size(acc)) then x else acc, sl')

List.foldl(String.size) ["This","the","A","Hello","World","not","long string","loooong string"];
				*)

fun longest_string_helper f l =
	let
		val h = fn (x, y) => if f(String.size(x), String.size(y))
							 then x
							 else y
	in
		List.foldl(h) "" l
	end;

(*
longest_string3 = fn : string list -> string
Behaves exactly like longest_string1.
*)
fun longest_string3(l:string list):string = longest_string_helper Int.> l;

(*
longest_string4 = fn : string list -> string
Behaves exactly like longest_string 2.
*)
fun longest_string4(l:string list):string = longest_string_helper Int.>= l;

(*
longest_capitalized = fn : string list -> string
Takes a string list and returns the longest string in the list that begins with
an uppercase letter (or "" if there are no such strings).
*)
fun longest_capitalized(l:string list): string = (longest_string3 o only_capitals) l;

(*
rev_string = fn : string -> string
Takes a string and returns the string that is the same characters in reverse
order.
*)
fun rev_string(s:string):string = (String.implode o List.rev o String.explode) s;

(*
first_answer = fn : ('a -> 'b option) -> 'a list -> 'b
The first argument should be applied to elements of the second argument in order,
until the first time it returns SOME v for some v and then v is the result of
the call to first_answer. If the first argument returns NONE for all list
elements, then first_answer raises the exception NoAnswer.
*)
fun first_answer f l =
	case l of
		[] => raise NoAnswer
	  | x::l' => if isSome (f x) then valOf(f x) else first_answer f l';

(*
all_answers = fn : ('a -> 'b list option) -> 'a list -> 'b list option
The first argument should be applied to elements of the second argument. If it
returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME
lstn and the result of all_answers is SOME lst where lst is lst1, lst2, ...,
lstn appended together (the order in the result list should be preserved).
*)
fun all_answers f l =
	let
		fun helper f l =
			case l of
				[] => []
			  | x::l' => if isSome(f x) then valOf(f x) @ helper f l' else raise NoAnswer

	in
		SOME(helper f l) handle NoAnswer => NONE
	end;

(*
count_wildcards = fn : pattern -> int
Takes a pattern and returns how many Wildcard patterns it contains.
*)
fun count_wildcards(p:pattern):int = g (fn () => 1) (fn x => 0) p;

(*
count_wild_and_variable_lengths = fn : pattern -> int
Takes a pattern and returns the number of Wildcard patterns it contains plus
the sum of the string lengths of all the variables in the variable patterns it
contains. Note: we only consider variable names; the constructor names are not
relevant.
*)
fun count_wild_and_variable_lengths(p:pattern):int = g (fn () => 1) (fn x => String.size(x)) p;

(*
count_some_var = fn : string * pattern -> int
Takes a string and a pattern (as a pair) and returns the number of times the
string appears as a variable in the pattern.
*)
fun count_some_var(s:string, p:pattern):int = g (fn () => 0) (fn x => if s = x then 1 else 0) p;

(*
check_pat = fn : pattern -> bool
Takes a pattern and returns true if and only if all the variables appearing in
the pattern are distinct from each other. The constructor names are not
relevant.
*)
fun check_pat(p) =
	let
		fun get_variables ps =
			case ps of
				[] => []
			  | x::t => (case x of Variable x => x::get_variables t
				  				| TupleP tp => (get_variables tp) @ (get_variables t)
								| ConstructorP(_,pt) => get_variables [pt]
								| _ => get_variables t)

		fun retrieve_strings(p) =
			case p of
				Variable x => [x]
			  | TupleP ps => get_variables ps
			  | ConstructorP(_,pt) => retrieve_strings pt
			  | _ => []

		fun find_duplicates strl =
			case strl of
				[] => true
			  | x::[] => true
			  | x::l => if (List.exists (fn y => y = x) l) then false else find_duplicates(l)
	in
		(find_duplicates o retrieve_strings) p
	end;

(*
match = fn : valu * pattern -> (string * valu) list option
Takes a valu * pattern and returns a (string * valu) list option,namely NONE if
the pattern does not match and SOME lst where lst is the list of bindings if it
does. Note that if the value matches but the pattern has no patterns of the
form Variable s, then the result is SOME [].
*)
fun match(v:valu, p:pattern) =
	let
		fun match_helper vs ps =
			case vs of
				[] => true
			  | v::l =>
					(case ps of
						[] => true
					  | x::t => (case x of Variable x => (match_helper l t)
					  					| Wildcard => (match_helper l t)
										| ConstP i => (case v of Const j => (j = i) andalso (match_helper l t) | _ => false)
										| TupleP tp => (case v of Tuple tpv => ((match_helper tpv tp) andalso (match_helper l t)) | _ => false)
										| ConstructorP(s,pt) => (case v of Constructor(sv,vv) => (match_helper [vv] [pt]) | _ => false)
										| UnitP => (case v of Unit => (match_helper l t) | _ => false)))
		fun can_match(v, p) =
			case p of
				Variable x => true
			  | Wildcard => true
		  	  | ConstP i => (case v of Const j => j = i | _ => false)
			  | UnitP => (case v of Unit => true | _ => false)
			  | TupleP ps => (case v of Tuple vs => (case vs of
				  								      [] => false
													| x::l => if ps = [] orelse (List.length(ps) <> List.length(vs))
													 		  then false
															  else match_helper vs ps)
									   | _ => false)
			  | ConstructorP (s,pt) => (case v of Constructor(st,pt') => (s = st) andalso can_match(pt',pt) | _ => false)
		fun traverse_tuple(vs, ps) =
			case vs of
				[] => []
			  | v::l => (case ps of [] => []
				  		  | x::l' => (case x of
							  			 Variable x' => [(x',v)] @ traverse_tuple(l, l')
									   | TupleP ps' => (case v of Tuple vs' => traverse_tuple(vs', ps') @ traverse_tuple(l, l')
									   							  | _ => [])
									   | _ =>  if can_match(v, x) then traverse_tuple(l, l') else []))
	in
		if can_match (v,p)
		then
			case p of
				Wildcard => SOME []
			  | Variable x => SOME [(x, v)]
			  | ConstP i => SOME []
			  | ConstructorP (s,pt) => (case v of Constructor (s, vpt) => match(vpt, pt) | _ => NONE)
			  | TupleP ps => (case v of Tuple vs => SOME (traverse_tuple(vs,ps)) | _ => NONE)
			  | UnitP => SOME []
		else NONE
	end;

(*
first_match = fn : valu -> pattern list -> (string * valu) list option
Takes a value and a list of patterns and returns a (string * valu) list option,
namely NONE if no pattern in the list matches or SOME lst where lst is the list
of bindings for the first pattern in the list that matches.
*)
fun first_match v lp = SOME (first_answer (fn pt => match(v, pt)) lp) handle NoAnswer => NONE
