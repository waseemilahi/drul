(*
***********************************************************************
*      			 DruL - Drumming Language
*
* Creation of R. Stewart, T. Bertin-Mahieux, W. Ilahi  and B. Warfield
*             rs2660      tb2332             wki2001       bbw2108
*
* for the class COMS W4115: Programming Language and Translators
*
* file: drul_types.ml
*
*       TYPES
*
* This file contains the internal type and exception declarations 
* required by the interpreter and printing/checking functions.
*
*************************************************************************
*)



open Drul_ast

module NameMap = Map.Make(String)

(* most of the exceptions *)
exception Type_error         of string
exception Invalid_function   of string
exception PatternParse_error of string
exception Invalid_argument   of string
exception Undefined_identifier of string
exception Illegal_assignment of string
exception Instruments_redefined of string

type pattern = bool list
type pattern_alias = bool array

(* type of every object in DruL *)
type drul_t =
		Void
	|	Int     of int
	|	Str     of string
	|	Bool    of bool
	|	Pattern of pattern
	|	Clip    of pattern array
	|	Mapper  of (string * string list * statement list)
	|	PatternAlias of pattern_alias
	|	Beat of pattern_alias * int
	|	Instruments of string list
	|	InstrumentAssignment of string * pattern

(*      symbol table for DruL
		the current environment is 'symbols': a map from string to drul_t,
		the parent is another drul_env
*)
type drul_env =
{
	symbols: drul_t NameMap.t;
	parent:  drul_env option
}
