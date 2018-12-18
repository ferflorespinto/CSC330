(*
Name: Jorge Fernando Flores Pinto
ID: V00880059
CSC330 – Fall 2018
Assignment 1
*)

(*
DATE type
Format: (year, month, day)
*)
type DATE = (int * int * int);
exception InvalidParameter

(*
val is_older = fn : DATE * DATE -> bool
Takes two dates as parameters and evaluates to true or false. It evaluates to
true if the first argument is a date that comes before the second argument.
If the two dates are the same, the result is false.
*)

fun is_older(d1: DATE, d2: DATE): bool =
  if #1 d1 < #1 d2
  then true
  else if #1 d1 = #1 d2
  then
    (* check for month *)
    if #2 d1 < #2 d2
    then true
    else if #2 d1 = #2 d2
    then #3 d1 < #3 d2
      (* check for day *)
    else false
  else false;

(*
val number_in_month = fn : DATE list * int -> int
Takes a list of dates and a month as parameters, and returns
how many dates in the list are in the given month.
*)
fun number_in_month(dlist: DATE list, m: int): int =
  if null dlist
  then 0
  else if #2 (hd dlist) = m
  then 1 + number_in_month(tl dlist, m)
  else number_in_month(tl dlist, m);

(*
val number_in_months = fn : DATE list * int list -> int
Takes a list of dates and a list of months as parameters and returns the
number of dates in the list of dates that are in any of the months in the list
of months. We assume the list of months has no number repeated.
*)
fun number_in_months(dlist: DATE list, mlist: int list): int =
  if null mlist
  then 0
  else number_in_month(dlist, hd mlist) + number_in_months(dlist, tl mlist);

(*
val dates_in_month = fn : DATE list * int -> DATE list
Takes a list of dates and a month as parameters, and returns a list holding the
dates from the argument list of dates that are in the month. The returned list
contains dates in the order they were originally given.
*)
fun dates_in_month(dlist: DATE list, m: int): DATE list =
  if null dlist
  then []
  else if #2 (hd dlist) = m
  then hd dlist :: dates_in_month(tl dlist, m)
  else dates_in_month(tl dlist, m);

(*
val dates_in_months = fn : DATE list * int list -> DATE list
Takes a list of dates and a list of months and returns a list holding the dates
from the argument list of dates that are in any of the months in the list of
months. We assume the list of months has no number repeated.
*)

fun dates_in_months(dlist: DATE list, mlist: int list): DATE list =
    if null mlist
    then []
    else dates_in_month(dlist, hd mlist) @ dates_in_months(dlist, tl mlist);

(*
val get_nth = fn : string list * int -> int
Takes a list of strings and an int n and returns the n-th element of
the list where the head of the list is 1st. Raise the exception
InvalidParameter if n is zero or larger than the length of the list.
*)
fun get_nth(slist: string list, n: int): string =
  if n = 0
  then raise InvalidParameter
  else let
    fun get_nth_helper(slist: string list, n: int, idx: int): string =
      if null slist
      then raise InvalidParameter
      else if n = idx
      then hd slist
      else get_nth_helper(tl slist, n, idx + 1)
    val idx = 1
  in
    get_nth_helper(slist, n, idx)
  end;

(*
val date_to_string = fn : DATE -> string
Takes a date and returns a string of the form January 20, 2013 (for example).
*)
fun date_to_string(d: DATE): string =
  let
    val months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 d) ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
  end;

(*
val number_before_reaching_sum = fn : int * int list -> int
Takes an int called sum, which we assume is positive, and an int list, which we
assume contains all positive numbers, and returns an int. We return an int n
such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. We also assume the entire list
sums to more than the passed in value; we raise an exception if this is not the
case.
*)
fun number_before_reaching_sum(sum: int, l: int list): int =
  let
    fun sum_helper(sum: int, l: int list, partial_sum: int, n: int): int =
      if partial_sum > sum orelse partial_sum = sum
      then n - 1
      else if null l
      then raise InvalidParameter
      else sum_helper(sum, tl l, partial_sum + hd l, n + 1)

  in
    sum_helper(sum, tl l, hd l, 1)
  end;

(*
val what_month = fn : int -> int
Takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.).
*)
fun what_month(day: int): int =
  let
    val from1to12 = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day, from1to12) + 1
  end;

(*
val month_range = fn : int * int -> int list
Takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn]
where m1 is the month of day1, m2 is the month of day1 + 1, ..., and mn is the
month of day day2. Note the result will have length day2 − day1 + 1 or length
0 if day1 > day2.
*)
fun month_range(day1: int, day2: int): int list =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2);

(*
val oldest = fn : DATE list -> DATE option
Takes a list of dates and evaluates to a DATE option.  It evaluates to NONE
if the list has no dates and SOME d if the date d is the oldest date in the
list.
*)
fun oldest(dlist: DATE list): DATE option =
    if null dlist
    then NONE
    else let
      fun find_oldest(dlist: DATE list, curr: DATE): DATE option =
        if null dlist
        then SOME (curr)
        else if is_older(curr, hd dlist)
        then find_oldest(tl dlist, curr)
        else find_oldest(tl dlist, hd dlist)
    in
      find_oldest(dlist, hd dlist)
    end;

(*
val reasonable_date = fn : DATE -> bool
Takes a date and determines if it describes a real date in the common era.
A “real date” has a positive year (year 0 did not exist), a month between 1 and
12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4
but not divisible by 100.
*)
fun reasonable_date(d: DATE): bool =
    if #1 d < 0 orelse #1 d = 0
    then false
    else if #2 d < 1 orelse #2 d > 12
    then false
    else let
      val months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
      val m = get_nth(months, #2 d)
      val leap_part1 = (#1 d) mod 400 = 0
      val leap_part2 = (#1 d) mod 4 = 0 andalso (#1 d) mod 100 <> 0
    in
      if m = "February"
      then
        if leap_part1 orelse leap_part2
        then #3 d > 0 andalso #3 d < 30
        else #3 d > 0 andalso #3 d < 29
      else if m = "January" orelse m = "March" orelse m = "May" orelse m = "July"
        orelse m = "August" orelse m = "October" orelse m = "December"
      then #3 d > 0 andalso #3 d < 32
      else #3 d > 0 andalso #3 d < 31
    end;
