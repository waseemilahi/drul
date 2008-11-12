open Drul_ast

(* module NameMap = Map.Make(String) *)

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

let evaluate e = match e with
(*	FunCall("print", [CStr(x)]) -> print_endline x *)
	CStr(x) -> print_endline x


in
let exec s = match s with
	Expr(e) -> evaluate e

in
let run p = match p with
	Content(statements) -> List.iter exec statements

in
let _ =
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
ignore (run programAst)
