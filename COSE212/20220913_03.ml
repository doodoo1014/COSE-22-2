let y = (1, "two", true);;

let fst p = 
  match p with
    | (fst, snd) -> fst
    (* # of elements of tuple p must be certain
    | (fst, snd, thr) -> thr
      error: This pattern matches values of type 'a * 'b * 'c
       but a pattern was expected which matches values of type 'd * 'e
    *)
    ;;

let snd p = match p with (_, snd) -> snd;;

fst(1, 2);;
fst(1, 2, 3);;
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
