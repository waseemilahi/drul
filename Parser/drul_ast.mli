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
	|   MemberCall of tagged_expr * string * tagged_expr list
	|   UnaryMinus of tagged_expr
	|   UnaryNot  of tagged_expr 
	|   ArithBinop of tagged_expr * intOp * tagged_expr
	|   LogicBinop of tagged_expr * boolOp * tagged_expr
	|   Comparison of tagged_expr * compOp * tagged_expr
	|   FunCall of string * tagged_expr list
	|   MapCall of mapper * tagged_expr list
	|	InstrAssign of string * tagged_expr
	|	Output of string * tagged_expr list

and statement =
	Expr of tagged_expr
	| Return of tagged_expr
	| Assign of string * tagged_expr * int
	| MapDef of string * string list * statement list * int
	| IfBlock of tagged_expr * statement list * statement list option
	| InstrDef of tagged_expr list * int
	| EmptyStat
and tagged_expr = { real_expr : expr ; lineno : int }

type program =
	Content of statement list
