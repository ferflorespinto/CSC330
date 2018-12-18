(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*
all_except_option = fn: string * string list -> string option.
Return NONE if the string is not in the list, else return SOME list where list
is identical to the argument list except the string is not in it. We assume the
string is in the list at most once.
*)
fun all_except_option(s:string, l:string list) =
  let
    fun all_except_helper(s1:string, l1:string list) =
      case l1 of
          [] => []
        | x::l1' => if (same_string(x, s1))
                    then l1'
                    else x::all_except_helper(s1, l1')

    val result = all_except_helper(s, l)
  in
    if (result = l)
    then NONE
    else SOME (result)
  end;

(*
get_substitutions1 = fn : string list list * string -> string list
Takes a string list list (a list of list of strings, the substitutions) and a
string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself is not in the
result.
*)
fun get_substitutions1(l_l:string list list, s: string) =
  case l_l of
      [] => []
    | x::l => let
                val result = all_except_option(s, x)
              in
                case result of
                    NONE => get_substitutions1(l, s)
                  | SOME x => x @ get_substitutions1(l, s)
              end;

(*
get_substitutions2 = fn : string list list * string -> string list
Same as get_substitutions1, but this function uses a tail-recursive solution.
*)
fun get_substitutions2(l_l:string list list, s: string) =
case l_l of
    [] => []
  | x::l => let
              val result = all_except_option(s, x)
              fun helper(l_h: string list, acc: string list) =
                case l_h of
                    [] => acc
                  | x_h::l_h' => x_h::helper(l_h', acc)
              in
                case result of
                    NONE => get_substitutions2(l, s)
                  | SOME x_r => helper(x_r, get_substitutions2(l, s))
              end;

(*
similar_names = fn : string list list * {first:string, middle:string, last:string}
  -> {first:string, middle:string, last:string} list
Takes a string list list of substitutions and a full name of type
{first:string,middle:string,last:string} and returns a list of full names (type
{first:string,middle:string,last:string} list). The result is all the full names
we can produce by substituting for the first name (and only the first name)
using substitutions. The answer should begin with the original name (then have
0 or more other names).
*)
fun similar_names(l_l:string list list, fullname:{first:string, middle:string, last:string}) =
  let
    fun construct_list(l:string list list, fname:{first:string, middle:string, last:string}) =
      case fname of {first=f, middle=m, last=la} => get_substitutions2(l, f)

    fun rec_helper(n:{first:string, middle:string, last:string}, name:string) =
      case n of {first=f, middle=m, last=l} => {first=name, middle=m, last=l}

    fun helper(l:string list, n:{first:string, middle:string, last:string}) =
      case l of
          [] => []
        | x::l' => rec_helper(n, x)::helper(l', n)

    val name_list = construct_list(l_l, fullname)
  in
    fullname::helper(name_list, fullname)
  end;

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(*
card_color = fn : card -> color
Takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red).
*)
fun card_color(c:card) =
  case c of
      (Clubs,_) => Black
    | (Diamonds,_) => Red
    | (Hearts,_) => Red
    | (Spades,_) => Black

(*
card_value = fn : card -> color
Takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10).
*)
fun card_value(c:card) =
  case c of
      (_,Num i) => i
    | (_,Jack) => 10
    | (_,Queen) => 10
    | (_,King) => 10
    | (_,Ace) => 11

(*
remove_card = fn : card list * card * exception -> card list
Takes a list of cards cs, a card c, and an exception e. It returns
a list that has all the elements of cs except c.  If c is in the list more than once, remove only the first
one. If c is not in the list, raise the exception e.
*)
fun remove_card(cs:card list, c:card, e) =
  case cs of
      [] => raise e
    | x::l' => if x = c then l' else x::remove_card(l', c, e)

(*
all_same_color = fn : card list -> bool
Takes a list of cards and returns true if all the cards in
the list are the same color. The color of empty list is true
*)
fun all_same_color(cs: card list) =
  let
    fun helper(cs':card list, acc:color) =
      case cs' of
        [] => true
      | x::l => if (card_color(x) = acc) then helper(l, acc) else false
  in
    case cs of
        [] => true
      | x::l => helper(l, card_color(x))
  end;

(*
sum_cards = fn : card list -> int
Takes a list of cards and returns the sum of their values.
*)
fun sum_cards(cs:card list) =
  let
    fun sum_helper(cs':card list, acc:int) =
      case cs' of
          [] => acc
        | x::l => sum_helper(l, (card_value(x) + acc))
  in
    sum_helper(cs, 0)
  end;

(*
score = fn : card list * int -> int
Takes a card list (the held-cards) and an int (the goal) and computes
the score.
*)
fun score(cs:card list, goal:int) =
  let
    val sum = sum_cards(cs)
    val pre_goal = if (sum > goal) then (sum - goal) * 2 else goal - sum
  in
    if (all_same_color(cs)) then pre_goal div 2 else pre_goal
  end;

fun officiate(cards:card list, moves:move list, goal:int) =
  let
    fun help_officiate(cs:card list, held:card list, ms:move list, g:int, sum:int) =
      case ms of
          [] => held
        | x::l => if sum < g orelse sum = g
                  then case x of
                        Discard c => help_officiate(cs, (remove_card(held, c, IllegalMove) handle IllegalMove => held), l, g, sum_cards(held))
                      | Draw => case cs of
                                    [] => held
                                  | x_cs::l_cs => help_officiate(l_cs, x_cs::held, l, g, sum_cards(x_cs::held))
                  else held
    val hand = help_officiate(cards, [], moves, goal, 0)
  in
    score(hand, goal)
  end;
