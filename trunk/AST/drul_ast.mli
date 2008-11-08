(* AST scratch *)

type intOp  = Add | Sub | Mult | Div | Mod

type compOp  = EqualTo | NotEqual | LessThan | GreaterThan | LessEq | GreaterEq

type boolOp = And | Or

type mapper =
    AnonyMap of statement list
    | NamedMap of string * string list
and expr = 
        Lit of int
    |   Var of string
    |   UnaryMinus of expr
    |   UnaryNot  of expr
    |   ArithBinop of expr * intOp * expr
    |   LogicBinop of expr * boolOp * expr
    |   Comparison of expr * compOp * expr
    |   FunCall of string * expr list
    |   MapCall of mapper * expr list
and statement =
    Expr of expr
    | Assign of string * expr
    | MapDef of string * string list * statement list
    | IfBlock of expr * statement list * statement list option

type program =
    Content of statement list