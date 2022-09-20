let rec sum l = 
  match l with
    | [] -> 0
    | hd::tl -> hd + (sum tl);;

let rec prod l = 
  match l with
    | [] -> 1
    | hd::tl -> hd * (prod tl);;

let rec fold_right f l a =
  match l with
    | [] -> a
    | hd::tl -> f hd (fold_right f tl a);;

let sum l = fold_right (fun x y -> x + y) l 0;;
sum[1;2;3;4];;

let sum l = fold_right (+) l 0;;
sum [1;2;3;4];;

let prod l = fold_right (fun x y -> x * y) l 1;;
prod [1;2;3;4];;


(* --------------------------------------- *)


let rec sum a l =
  match l with
    | [] -> a
    | hd::tl -> sum (a+hd) tl;;
    
let rec prod a l =
  match l with
    | [] -> a
    | hd::tl -> prod (a*hd) tl;;
    
let rec fold_left f a l =
  match l with
    | [] -> a
    | hd::tl -> fold_left f (f a hd) tl;;
    
let sum a l = fold_left (fun x y -> x + y) a l;;
sum 0 [1;2;3;4];;

let prod a l = fold_left (fun x y -> x * y) a l;;
prod 1 [1;2;3;4];;
