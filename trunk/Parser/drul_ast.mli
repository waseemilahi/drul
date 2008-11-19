(* AST scratch *)

type intOp  = Add | Sub | Mult | Div | Mod

type compOp  = EqualTo | NotEqual | LessThan | GreaterThan | LessEq | GreaterEq

type boolOp = And | Or

type mapper =
    AnonyMap of statement list
    | NamedMap of string
and expr = 
        CInt of int
    |   CStr of string
    |   CBool of bool
    |   Var of string
    |   MemberCall of expr * string * expr list
    |   UnaryMinus of expr
    |   UnaryNot  of expr
    |   ArithBinop of expr * intOp * expr
    |   LogicBinop of expr * boolOp * expr
    |   Comparison of expr * compOp * expr
    |   FunCall of string * expr list
    |   MapCall of mapper * expr list
    |   MakeClip of clip_elem list

and statement =
    Expr of expr
    | Return of expr
    | Assign of string * expr
    | MapDef of string * string list * statement list
    | IfBlock of expr * statement list * statement list option
    | EmptyStat


and clip_elem = 
    InstrAssign of string * string

type program =
    Content of statement list
