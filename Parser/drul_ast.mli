(* AST scratch *)

type intOp  = Add | Sub | Mult | Div | Mod

type compOp  = EqualTo | NotEqual | LessThan | GreaterThan | LessEq | GreaterEq

type boolOp = And | Or

type mapper =
		AnonyMap of statement list
	|	NamedMap of string

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
	|	InstrAssign of string * expr
	|	Output of string * expr list

and statement =
	Expr of tagged_expr
	| Return of tagged_expr
	| Assign of string * tagged_expr * int
	| MapDef of string * string list * statement list * int
	| IfBlock of tagged_expr * statement list * statement list option
	| InstrDef of expr list * int
	| EmptyStat
and tagged_expr = { real_expr : expr ; lineno : int }

type program =
	Content of statement list
