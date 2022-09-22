type program = exp
and exp = 
  | CONST of int
  | VAR of string
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of string * exp * exp
  | READ

let p1 =
  LET ("x", CONST 1,
    ADD (VAR "x", CONST 2))

let p2 =
  LET ("x", CONST 1,
    LET ("y", CONST 2,
      ADD (VAR "x", VAR "y")))

let p3 =
  LET ("x", LET ("y", CONST 2,
    ADD (VAR "y", CONST 1)),
  ADD (VAR "x", CONST 3))

let p4 =
  LET ("x", CONST 1,
    LET ("y", CONST 2,
      LET ("x", CONST 3,
        ADD (VAR "x", VAR "y"))))

let p5 =
  LET ("x", CONST 1,
    LET ("y", LET ("x", CONST 2,
      ADD (VAR "x", VAR "x")),
      ADD (VAR "x", VAR "y")))

let p6 =
  LET ("x", CONST 1,
    LET ("y", CONST 2, 
      IF (ISZERO (SUB (VAR "x", CONST 1)),
        SUB (VAR "y", CONST 1),
        ADD (VAR "y", CONST 1))))

let p7 =
  LET ("x", CONST 1,
    LET ("y", ISZERO (VAR "x"),
      ADD (VAR "x", VAR "y")))
    
(* Abstract Syntax Tree *)


let p8 =
  LET ("x", CONST 7,
    LET ("y", CONST 2,
      LET ("y", LET ("x", SUB(VAR "x", CONST 1),
        SUB (VAR "x", VAR "y")),
    SUB (SUB (VAR "x", CONST 8), VAR "y"))))


  
