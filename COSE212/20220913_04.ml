let fst l = 
  match l with
    hd::_ -> hd;;
    
fst [1; 2; 3];;
fst [1; 2];;
fst [1; 2; 3; 4; 5];;

(* compare with tuple and list *)
(1, "two", true);;
(*
[1; "two"; true];;
error: This expression has type string but an expression was expected of type int
because Ocaml static type system is not complete 
*)

[1; 2] = [2; 1];;
[1; 2] = [1; 2];;
[1; 2] == [1; 2];;
(* result is false because == means comparing "physically same (memory)" *)


let hd::tl = [1; 2; 3];;
let a::b = [1; 2; 3];;

1::[2; 3];;
1::(2::[3]);;
1::(2::(3::[]));;
1::2::3::4::5::6::[];;
(* cons operator(::): add a single element to the front of a list *)
[[1]; [1;2]; [1;2;3]; [1;2;3;4]];;
[1; 2] @ [3; 4; 5];;


(* -------------------------------------------------------------------------- *)


let isnil l =
  match l with
    | [] -> true
    |_ -> false;;

let isnil l =
  match l with
    | [] -> true
    | hd::tl -> false;;
    
let isnil l =
  match l with
    | hd::tl -> false
    | _ -> true;;
    
isnil [];;
isnil [1; 2];;

let rec length l = 
  match l with
    | [] -> 0
    | h::t -> 1 + length t;;

length [1; 2; 3];;

let rec length l =
  match l with
    | [] -> 0
    | _::t -> (length t) + 1;;
    
length [[1;2]; [1;2;3]; []; [-1]];;


(* -------------------------------------------------------------------------- *)


let rec even_cnt : int list -> int
= fun l ->
  match l with
    | [] -> 0
    | hd::tl -> (even_cnt tl) + (if hd mod 2 = 0 then 1 else 0);;

even_cnt [1; 2; 3; 4; 5; 6];;
    
let rec count : (int -> bool) -> int list -> int
= fun test l ->
  match l with
    | [] -> 0
    | hd::tl -> (count test tl) + (if test hd then 1 else 0);;

count (fun x -> x mod 2 = 0) [1; 2; 3];;
count (fun x -> x mod 2 = 1) [1; 2; 3];;
count (fun x -> x >= 0) [-2; -1; 0; 1; 2];;
