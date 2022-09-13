let square : int -> int
= fun x -> x * x

let sum_if_true : (int -> bool) -> int -> int -> int
= fun test first second ->
  (if test first then first else 0) + 
  (if test second then second else 0);;
