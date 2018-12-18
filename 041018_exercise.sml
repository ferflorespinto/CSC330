(*
In class exercise (04/10/18)
After evaluating the following bindings:
 - What is the type of the f binding?
 - What is the type of ans?
*)

val x = 1
(*
val f = fn : (int -> 'a) -> 'a
*)
val f = (fn y => y x)
val x = 7
val g = (fn y => x - y)
val ans = f g

(*
Results:
val x = <hidden-value> : int (this is because line 13 shadows line 8)
val f = fn : (int -> 'a) -> 'a
val x = 7 : int
val g = fn : int -> int
*)
