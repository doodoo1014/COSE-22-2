let y = (1, "two", true);;

let fst p = 
  match p with
    | (fst, snd) -> fst
    ;;

let snd p = match p with (_, snd) -> snd;;

fst(1, 2);;
snd(3, "true");;


let p = (1, true);;
let (x, y) = p;;


(* two integer of one tuple? *)
let mult1 x y = x * y;;
let mult2 (x, y) = x * y;;

mult1 1 2;;
mult2 (1, 2);;

(* mult2 1 2;; *)
(* error: because it mult2's input is tuple, but real input is two integers *)
