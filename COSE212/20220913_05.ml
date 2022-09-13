type days = Mon | Tue | Wed | Thu | Fri | Sat | Sun;;
Mon;;

let nextday : days -> days
= fun day -> match day with
  | Mon -> Tue
  | Tue -> Wed
  | Wed -> Thu
  | Thu -> Fri
  | Fri -> Sat
  | Sat -> Sun
  | Sun -> Mon;;

nextday Mon;;

type shape = Rect of int * int | Circle of int;;
Rect (10, 5);;
Circle 5;;

let area : shape -> float
= fun s -> match s with
  | Rect (w, h) -> float_of_int (w * h)
  | Circle r -> (float_of_int r) *. (float_of_int r) *. 3.141592;;

area (Circle 5);;
area (Rect (4, 5));;


(* ----------------------------------------------------------------- *)


type mylist = Nil | List of int * mylist;;
Nil;;
List (1, Nil);; (* Ocaml notation: [1] *)
List (1, List (2, Nil));; (*Ocaml notation: [1; 2] *)
List (1, List (2, List (3, Nil)));; (*Ocaml notation: [1; 2; 3] *)

let rec mylength : mylist -> int
= fun l ->
  match l with
    | Nil -> 0
    | List (hd, tl) -> mylength tl + 1;;

mylength Nil;;
mylength (List (1, List (2, List (3, Nil))));;

type tree = Leaf | Node of int * tree * tree;;
Leaf;;
Node (2, Leaf, Leaf);;
Node (1, Node (2, Leaf, Leaf), Leaf);;

let t4 = Node (1, Node (2, Leaf, Leaf), Node (3, Node (4, Leaf, Leaf), Leaf));;

let rec count : tree -> int
= fun t ->
  match t with
    | Leaf -> 0
    | Node (n, t1, t2) -> (count t1) + (count t2) + 1;;

count t4;;

let rec sum : tree -> int
= fun t ->
  match t with
    | Leaf -> 0
    | Node (n, t1, t2) -> (sum t1) + (sum t2) + n;;
    
sum t4;;
