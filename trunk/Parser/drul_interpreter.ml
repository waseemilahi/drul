open Drul_ast

module NameMap = Map.Make(String)

exception Type_error of string

type pattern = bool list

type t = Void
	   | Int of int
	   | Str of string
	   | Bool of bool
	   | Pattern of pattern
	   | Clip of pattern array

(*
let rec execPrint env = function
	  []       -> print_endline ""; (CInt(0), Void)
	| head::[] -> let (exprVal, typeVal) = (eval env head) in match (exprVal, typeVal) with
			(expr, Str) -> print_endline expr; (CInt(0), Void)
		  | _           -> raise (Failure ("ERROR: Print can only be used on strings"))
	| _ -> raise (Failure ("ERROR: Print can take a single argument"))

let evaluate env = function
	  CStr(x)                     -> (CStr(x), Str)
	| FunCall("print", [CStr(x)]) -> print_endline x; 0
in
let exec env = function
	Expr(e) -> let _ = evaluate env e; ignore()

in
let run = function
	Content(statements) -> List.fold_left exec NameMap.empty statements; ignore()
*)

let rec evaluate e env = match e with
		FunCall("print", [arg]) -> let v = evaluate arg env in
		(
			match v with
			Str(x) -> print_endline x; Void
			| Int(y) -> print_endline (string_of_int y); Void
			| Bool(z) -> print_endline( if z then "TRUE" else "FALSE" ); Void
			| _ -> print_endline("Dunno how to print this yet."); Void
		)
	|	CStr(x) -> Str(x)
	|   CBool(x) -> Bool(x)
	|   CInt(x) -> Int(x)
	|	UnaryMinus(xE) -> let xV = evaluate xE env in
		( 
			match xV with
				Int(x) -> Int(-x)
			| _ -> raise (Type_error "you can't negate that, dorkface")
		)
	|   UnaryNot(xE) ->  let xV = evaluate xE env in
		(
			match xV with
				Bool(x) -> Bool(not x)
			| _ -> raise (Type_error "you can't contradict that, dorkface")
		)
	|	ArithBinop(aExp, operator, bExp) ->
		let aVal = evaluate aExp env in
		let bVal = evaluate bExp env in
		(
			match (aVal, operator, bVal) with
				(Int(a), Add, Int(b)) -> Int(a + b)
			|	(Int(a), Sub, Int(b)) -> Int(a - b)
			|	(Int(a), Mult, Int(b)) -> Int(a * b)
			|	(Int(a), Div, Int(b)) -> Int(a / b)
			|	(Int(a), Mod, Int(b)) -> Int(a mod b)
			| _ -> raise (Type_error("cannot do arithmetic on non-integers"))
		)
	|	LogicBinop(aExp, operator, bExp) -> 
		let aVal = evaluate aExp env in
		let bVal = evaluate bExp env in
		(
			match (aVal, operator, bVal) with
				(Bool(x), And, Bool(y)) -> Bool(x && y)
			|	(Bool(x), Or,  Bool(y)) -> Bool(x || y)
			| _ -> raise (Type_error "cannot do logical operations except on booleans")
		)
	|	Comparison(aExp, operator, bExp) ->
		let aVal = evaluate aExp env in
		let bVal = evaluate bExp env in
		(
			match (aVal, operator, bVal) with
				(Int(a), LessThan, Int(b)) -> Bool(a < b)
			|	(Int(a), GreaterThan, Int(b)) -> Bool(a > b)
			|	(Int(a), LessEq, Int(b)) -> Bool(a <= b)
			|	(Int(a), GreaterEq, Int(b)) -> Bool(a >= b)
			|	(Int(a), EqualTo, Int(b)) -> Bool(a == b)
			|	(Int(a), NotEqual, Int(b)) -> Bool(a != b)
			| _ -> raise (Type_error "cannot do that comparison operation")
		)
		
	| _ -> Void

let execute s env = match s with
	Expr(e) -> ignore(evaluate e env)
	| _ -> ignore()

let run p env = match p with
	Content(statements) -> List.iter (fun s -> execute s env) statements

let _ =
let unscoped_env = NameMap.empty in
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
ignore (run programAst (unscoped_env, None))

