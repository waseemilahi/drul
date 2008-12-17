(*
***********************************************************************
*      			 DruL - Drumming Language
*
* Creation of R. Stewart, T. Bertin-Mahieux, W. Ilahi  and B. Warfield
*             rs2660      tb2332             wki2001       bbw2108
*
* for the class COMS W4115: Programming Language and Translators
*
* file: drul_interpreter.ml
*
*       				INTERPRETER
*
* This file contains the interpreter for DruL. It receives an AST
* and interprets the code.
* This code is written in OCaml.
*
************************************************************************
*)

open Drul_ast
open Drul_main
open Drul_types

let run p env = match p with
	Content(statements) -> Random.self_init(); ignore(execlist statements env)

let _ =
let unscoped_env = {symbols = NameMap.empty; parent = None} in
let arglen = Array.length Sys.argv in
let input_stream = if 1 < arglen then open_in Sys.argv.(1) else stdin in
let lexbuf = Lexing.from_channel input_stream in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
try run programAst unscoped_env
with Type_error(msg,line)         -> Printf.fprintf stderr "Type error on line %d: %s\n" line msg
| Invalid_function(msg,line)      -> Printf.fprintf stderr "Invalid function call on line %d: %s\n" line msg
| PatternParse_error(msg,line)    -> Printf.fprintf stderr "Invalid pattern string on line %d: %s\n" line msg
| Invalid_argument(msg,line)      -> Printf.fprintf stderr "Incorrect function arguments on line %d: %s\n" line msg
| Undefined_identifier(msg,line)  -> Printf.fprintf stderr "Reading undefined identifier '%s' attempted on line %d.\n" msg line
| Illegal_assignment(msg,line)    -> Printf.fprintf stderr "Illegal assignment attempted on line %d: %s\n" line msg
| Instruments_redefined(msg,line) -> Printf.fprintf stderr "Instrument redefinition attempted on line %d: %s\n" line msg
| Failure(msg)                    -> Printf.fprintf stderr "Untrapped internal error! (error message: %s)\n" msg
