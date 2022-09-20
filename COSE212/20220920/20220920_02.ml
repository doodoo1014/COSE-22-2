let rec length l =
  match l with
    | [] -> 0
    | hd::tl -> 1 + length tl;;

let rec reverse l =
  match l with
    | [] -> []
    | hd::tl -> (reverse tl) @ [hd];;
    
let rec is_all_pos l =
  match l with
    | [] -> true
    | hd::tl -> (hd > 0) && (is_all_pos tl)

(* ---------------------------------------- *)

let rec fold_right f l a =
  match l with
    | [] -> a
    | hd::tl -> f hd (fold_right f tl a);;

let rec fold_left f a l =
  match l with
    | [] -> a
    | hd::tl -> fold_left f (f a hd) tl;;
    
let length l = fold_right (fun x y -> 1 + y) l 0;;
length [1;2;3;4];;

let length l = fold_left (fun x _ -> x + 1) 0 l;;
length [1;2;3;4];;

let reverse l = fold_right (fun x y -> y@[x]) l [];;
reverse [1;2;3;4];;

let reverse l = fold_left (fun x y -> y::x) [] l;;
reverse [1;2;3;4];;

let is_all_pos l = fold_right (fun x y -> (x>0) && y) l true;;
is_all_pos [1;2;3;4];;
is_all_pos [1;3;-2;4];;

let is_all_pos l = fold_left (fun x y -> x && (y>0)) true l;;
is_all_pos [1;2;3;4];;
is_all_pos [1;3;-2;4];;

let square x = x * x;;
let cube x = x * x * x;;

let map f l = fold_right (fun x y -> (f x)::y) l [];;
map square [1;2;3;4];;
map cube [1;2;3;4];;

let map f l = fold_left (fun x y -> x@[f y]) [] l;;
map square [1;2;3;4];;
map cube [1;2;3;4];;
