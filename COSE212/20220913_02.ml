let id x = x;;
(* polymorphic types *)

id 1;;
id "abc";;
id true;;

let first_if_true test x y
= if test x then x else y;;

let even x = if x mod 2 = 0 then true else false;;

first_if_true even 3 4;;

let second_if_true test x y z
= if test x then x else y;;
