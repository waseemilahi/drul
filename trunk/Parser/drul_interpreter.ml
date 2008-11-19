open Drul_ast

module NameMap = Map.Make(String)

(* type t = Void | Int | Str | Bool | Pattern | Clip *)

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

let evaluate e env = match e with
		FunCall("print", [CStr(x)]) -> print_endline x
	|	FunCall("print", [CInt(y)]) -> print_endline (string_of_int y)
	|	FunCall("print", [CBool(z)]) -> print_endline( if z then "TRUE" else "FALSE" )
	

let execute s env = match s with
	Expr(e) -> evaluate e env

let run p env = match p with
	Content(statements) -> List.iter (fun s -> execute s env)  statements


let _ =
let unscoped_env = NameMap.empty in
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
ignore (run programAst (unscoped_env, None))

