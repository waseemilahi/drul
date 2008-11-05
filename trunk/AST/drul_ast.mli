(* AST scratch *)

type program =
	statements

type intOp  = Add | Sub | Mult | Div | Mod

type compOp  = EqualTo | NotEqual | LessThan | GreaterThan | LessEq | GreaterEq

type boolOp = And | Or

type expr = 
		Lit of int
	|	Var of string
	|   UnaryMinus of expr
	|   UnaryNot  of expr
	|	ArithBinop of expr * arithOp * expr
	| 	LogicBinop of expr * boolOp * expr
	|   Comparison of expr * compOp * expr
	|	FunCall of string * arglist
	|	MapCall of mapper * arglist


type mapper =
	AnonyMap of statements
	| NamedMap of string * string list

type arglist =
	Args of expr list (* or something like that *)

type statements =
	Statements of statement list (* we still don't remember exactly the syntax for this *)

type statement =
	Expr of expr
	| Assign of Var * expr
	| MapDef of string * string list * statement
	| IfBlock of expr * statements * statements option

