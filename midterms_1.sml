(*
Solving sample midterms.
Midterm 1:
*)
(*
Question 1 (a)
f = fn : ('a -> int) -> 'a -> int -> int
ans = 27
*)
fun f x y z = x(y) + z
val y = 3
fun g z =
    let
        val x = fn x => x * 2
    in
        f z
    end
val h = g (fn a => a * a) (* f (fn a => a*a) *)

val ans = h 5 2 (* ans = 27 *)

(*
Question 1 (b)
f = fn : (int * int) list -> int
ans = 9
*)
fun f x =
    case x of
        [] => 0
      | (a,b)::[] => a + b
      | (a,b)::(c,d)::e => a + d + f(e)

(*val preans = map (fn x => (1,x), [2,3,4]);
val ans = f preans;*)
(*
map => [(1,2), (1,3), (1,4)]
ans = 1 + 3 + 1 + 4 = 9
*)

(*
Question 1 (c)
f = fn : int -> int
ans = 17
*)
val x = 7;
fun g y = x * y;
fun f z =
    let
        val x = 3
    in
        g(z) + x
    end
val ans = f(2)
(*
f(2) = g(2) + 3 = (7 * 2) + 3 = 14 + 3 = 17
ans = 17
*)

(*
Question 2 (a)
*)
fun addAllOpt (ol:int option list):int option =
    let
        fun checkIfAllNone optl =
            case optl of
                [] => true
              | x::l => if isSome(x) then false else checkIfAllNone l
        val all_none = checkIfAllNone ol
        val get_ints = fn opt =>
            case opt of
                NONE => 0
              | SOME x => x
        val int_list = List.map get_ints ol
        val res = List.foldl (fn (x,y) => x + y) 0 int_list
    in
        if all_none then NONE else SOME(res)
    end;

(*
Question 2 (b)
*)
fun fold (f, acc, xs) =
    case xs of
        [] => acc
      | x::xs' => fold(f, f(acc,x), xs');

fun map(f, l) =
    fold(fn (acc,x) => acc @ [f(x)], [], l)


datatype tree = EmptyT
            | Tree of (int * tree * tree)
(*
Question 3 (a)
*)
fun insert(t:tree, v:int):tree =
    case t of
        EmptyT => Tree (v, EmptyT, EmptyT)
      | Tree(x,left,right) => if v > x then Tree(x, left, insert(right, v))
                            else Tree(x, insert(left,v), right);

(*
Question 3 (b)
*)
fun fold_tree f acc t =
    case t of
        EmptyT => acc
      | Tree(x,left,right) => let
                                val l = fold_tree f acc left
                                val m = f(l, x)
                               in
                                fold_tree f m right
                               end;

fun to_string (acc, i) =
    if acc = ""
    then Int.toString(i)
    else acc ^ " " ^ Int.toString(i);

val tree_to_string = fold_tree to_string "";
val tr = insert(insert(insert(EmptyT,3),2),5);
val trToStr = tree_to_string tr;
(*val getTreeVal = fn tr' => (case tr' of EmptyT => 0 | Tree(x,l,r) => x)
val treeToList = fn (acc,tr') => acc @ [getTreeVal tr']*)
val sum = fn (acc, x) => acc + x;
val getsum = fold_tree sum 0 tr;

fun fib1 n =
    if n < 2 then 1 else fib1(n-1) + fib1(n-2);

fun fib2 n =
    let
        fun fib_helper(prev, curr, i, n) =
            if i > n
            then curr
            else fib_helper(curr, curr + prev, i + 1, n)
    in
        fib_helper(0, 1, 1, n)
    end;

(****************************** Midterm 2 *************************************)

(*
Question 1 (a)
f = fn : (int -> 'a) -> 'a
ans = 6
*)
val x = 1
(*
f = fn : (int -> 'a) -> 'a
*)
val f = (fn y => y x)
val x = 7
val g = (fn y => x - y)
val ans = f g (* f g => g x => x - x => 7 - 1 = 6 *)

(*
Question 1 (b)
f = fn : ('a -> int) * (int -> 'a) -> int
ans = 12
*)
(*
f = fn : ('a -> int) * (int -> 'a) -> int
*)
fun f p =
    let
        val x = 3
        val y = 4
        val (z,w) = p
    in
        (z (w y)) + x
    end

val x = 1
val y = 2
val ans = f((fn z => x + z), (fn x => x + x + 0))
(* (4 + 4 + 0) = 8 => 1 + 8 = 9 => 9 + 3 = 12 *)

(*
Question 1 (c)
f = fn : 'a list list -> 'a list
ans = [1,4]
*)
val f = fn x => List.map hd x
val ans = f [[1,2,3],[4,5]]

(*
Question 2 (a)
*)
fun rev l =
    let
        fun revh acc ls =
            case ls of
                [] => acc
              | x::ls' => revh (x::acc) ls'
    in
        revh [] l
    end;

fun split l =
    let
        fun splith (sl1, sl2, ls, i) =
            case ls of
                [] => (sl1, sl2)
              | x::t => if (i mod 2) = 0
                        then splith(sl1, sl2 @ [x], t, i + 1)
                        else splith(sl1 @ [x], sl2, t, i + 1)
    in
        splith([],[],l,1)
    end;

fun map (f,xs) =
    case xs of
        [] => []
      | x::xs' => (f x) :: map (f, xs')

fun listify l =
    map (fn x => [x], l); 
