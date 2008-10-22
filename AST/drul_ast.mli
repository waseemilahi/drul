(* AST scratch *)

type program =
	statements

type operator = Add | Sub | Mult | Div

type expr = 
		Lit of int
	|	Var of string
	| 	Binop of expr * operator * expr
	|	FunCall of string * arglist
	|	MapCall of mapper * arglist


type mapper =
	AnonyMap of statements
	| NamedMap of string

type arglist =
	Args of List (* or something like that *)
		(* list of expressions, yes? *)

type statements =
	Oops of List (* we still don't remember exactly the syntax for this *)

type statement =
	Expr of expr
	| Statements of statement * statements
	| Assign of Var * expr
	| MapDef of string * arglist * statement
	| IfBlock of expr * statement * statement option


