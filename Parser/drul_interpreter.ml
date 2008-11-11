open Drul_ast

module NameMap = Map.Make(string)

type t = Int | String | Bool | Pattern | Clip
type tval = expr * t

let rec execPrint env = function
	  []       -> print_endline ""
	| head::[] -> print_endline (eval head)
	| _        -> raise (Failure ("ERROR: Print can take a single argument"))

and execFun env = function
	("print", eList) -> execPrint env eList

and eval env = function
	  CInt(x)  -> (x, Int)
	| CStr(x)  -> (x, String)
	| CBool(x) -> (x, Bool)
	| FunCall(name, eList) -> execFun name env eList

in
let rec exec env = function
	Expr(e) -> eval env e

in
let run = function
	Content(statements) -> List.fold_left exec StringMap.empty statements

let _ =
let lexbuf = Lexing.from_channel stdin in
let program = Drul_parser.program Drul_scanner.token lexbuf in
let result = run program in
print_endline (string_of_int result)
