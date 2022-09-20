let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun p l ->
  match l with 
    | [] -> []
    | hd::tl ->
      if p hd then hd::(filter p tl)
      else (filter p tl);;

let even = filter (fun x -> x mod 2 = 0);;

even [1;2;3;4;5];;
