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
let unscoped_env = {symbols = NameMap.empty; parent= None} in
let arglen = Array.length Sys.argv in
let input_stream = if 1 < arglen then open_in Sys.argv.(1) else stdin in
let lexbuf = Lexing.from_channel input_stream in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
run programAst unscoped_env;;
