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
*       INTERPRETER
*
* This file contains the interpreter for DruL. It receives an AST
* and interprets the code.
* This code is written in OCaml.
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

type pattern = bool list
type pattern_alias = bool array

(*      type of every object in DruL
*)
type drul_t = Void
	   | Int     of int
	   | Str     of string
	   | Bool    of bool
	   | Pattern of pattern
	   | Clip    of pattern array
	   | Mapper  of (string * string list * statement list)
	   | PatternAlias of pattern_alias
	   | Beat of pattern_alias * int


(*      symbol table for DruL
        the current environment is 'symbols': a map from string to drul_t,
        the parent is another drul_env
*)   
type drul_env = 
	{
		symbols: drul_t NameMap.t;
		parent:  drul_env option
	}

(* exception used to handle return statement, similar to MicroC from Edwards
*)
exception Return_value of drul_env



(*
	turn a pattern object (list of booleans) into an array, and return
	pairs of (array, alias) to be added to the symbol table
*)
let rec get_alias_list p_list a_list counter =
	let newcounter = counter + 1 in
		match (p_list,a_list) with
		([],[]) -> []
	|	([],oops) -> raise (Failure "not enough patterns provided to mapper")
	|	(thispat::rest,[]) ->
(thispat, "$" ^ string_of_int counter) :: get_alias_list rest [] newcounter
	|	(thispat::rest,thisalias::other_aliases) ->
			let dollar_alias =  "$" ^ (string_of_int counter) in
			[(thispat, dollar_alias); (thispat, thisalias)]
			@ get_alias_list rest other_aliases newcounter

(*
	given a NameMap and a (pattern, alias) pair,
	add the appropriate information to the NameMap
	(at this point, an array of the beats is the pattern)
*)
let add_pattern_alias  symbol_table pair =
	let p_obj = fst(pair) in
	let alias = snd(pair) in
	let p_list = (match p_obj with Pattern(pat)->pat | _ -> raise (Failure "erp")) in
	let p_array = Array.of_list p_list in
	let beat_holder = PatternAlias(p_array)
	in NameMap.add alias beat_holder symbol_table
 
(*
	use the above functions to add the correct entries to a new symbol table
	before entering a "map" block
*)
let init_mapper_st p_list a_list =
	let alias_list = get_alias_list p_list a_list 1
	in List.fold_left add_pattern_alias NameMap.empty alias_list

(* create a new symbol table with the appropriate aliases, and link it to the parent *)
let get_map_env parent_env p_list a_list =
	let new_symbol_table = init_mapper_st p_list a_list 
	in {symbols = new_symbol_table; parent = Some(parent_env)}

(* is called by find_longest_list *)
let maxlen_helper currmax newlist =
	match newlist with 
	Pattern(patlist) -> (
		let currlen = List.length patlist in
		if (currlen > currmax) then currlen else currmax 
	)
	| _ -> raise (Failure "asshole")

(* find the length of the longest list *)
let find_longest_list patternlist =
	List.fold_left maxlen_helper 0 patternlist
	
(* add a given key & value to env in (env,parentenv) *)
let add_key_to_env env key value = 
	match env with {symbols=old_st;parent=whatever} -> 
		let new_st = NameMap.add key value old_st
		in {symbols = new_st; parent = whatever}
		
(* retrieve the value for a given key from the environment
	or its parent.
   If the value is a PatternAlias, then use some magic to transform 
   it into a Beat
*)
let rec get_key_from_env env key =
	if NameMap.mem key env.symbols then NameMap.find key env.symbols
	else match env.parent with 
			Some(parent_env) -> get_key_from_env parent_env key
		|	None -> raise (Undefined_identifier key)
and  beat_of_alias env alias =
	let currentVar = get_key_from_env env "$current"
	in match currentVar with 
			Int(currentVal) -> Beat(alias,currentVal)
		|	_	-> raise (Failure "Can't have a non-integer in $current--you really can't!!")


let state_of_beat beat =
	match beat with (pattern_data,idx) ->
	let pattern_length = Array.length pattern_data in
	if (idx < 0 or idx >= pattern_length) then None else Some(pattern_data.(idx))

(* inside a map, do one step!
   return is saved as "return" in the env
   current index is saved as "$current" in the env
*)
let rec one_mapper_step maxiters current st_list env current_pattern =
	if (maxiters == current) then Pattern(current_pattern)
	else
		let retval = Pattern([]) in 
		let env = add_key_to_env env "return" retval 	in 
		let env = add_key_to_env env "$current" (Int(current)) in 
		let newenv = execlist_returning st_list env in
		let new_st = newenv.symbols in
		let return = NameMap.find "return" new_st in
		let current_pattern = (match return with 
			Pattern(bools) -> current_pattern @ bools 
			| _ -> (raise (Failure "Jerks")) )
		in
		let current = current + 1 in
		one_mapper_step maxiters current st_list newenv current_pattern

(* run a named mapper, find the mapper in the env, and cast it to a
 anonymous mapper
*)
and run_named_mapper mapname argList env = 
  let savedmapper = get_key_from_env env mapname in
    match savedmapper with 
	Mapper(mapname2,strlist,stat_list) -> 
	  (* check if we receive the good number of patterns *)
	  if List.length strlist != List.length argList  then raise (Invalid_argument "wrong number of inputs for named mapper")
	  else run_mapper stat_list argList env
      (* if given name is not bound to a mapper, Type_error *)
      | _ -> raise (Type_error "we were expecting a mapper, name associated with something else")

(* main function of a map, takes a list of statement (body of the mapper)
   evaluate the arg_list, which should be a list of patterns
   launches the iteration (one_mapper_step)
*)
and run_mapper statement_list arg_list env = 
    let arg_list_evaled = eval_arg_list arg_list env in
	let map_env = get_map_env env arg_list_evaled [] in (* FIXME: alias list from mapdef *)
	let max_iters = find_longest_list arg_list_evaled in
	one_mapper_step max_iters 0 statement_list map_env []

(* evaluate an expr_list when we know that they're all patterns *)
and eval_arg_list arg_list env = match arg_list with
		[] -> []
	| 	headExp::tail -> (
			let headVal =  evaluate headExp env
			in headVal :: (eval_arg_list tail env)
		)

and evaluate e env = match e with
		FunCall(fname, fargs) -> function_call fname fargs env
	|	MemberCall(objectExpr, mname, margs) -> member_call objectExpr mname margs env
	|	CStr (x) -> Str (x)
	|   CBool(x) -> Bool(x)
	|	CInt (x) -> Int (x)
	|	Var(name) -> let fetched = get_key_from_env env name in
			(match fetched with
					PatternAlias(alias) -> beat_of_alias env alias
				|	other -> other
			)
	|	UnaryMinus(xE) -> let xV = evaluate xE env in
		(
			match xV with
				Int(x) -> Int(-x)
			| _ -> raise (Type_error "you can't negate that, dorkface")
		)
	|	UnaryNot(xE) ->  let xV = evaluate xE env in
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
				(Int(a), Add,  Int(b)) -> Int(a + b)
			|	(Int(a), Sub,  Int(b)) -> Int(a - b)
			|	(Int(a), Mult, Int(b)) -> Int(a * b)
			|	(Int(a), Div,  Int(b)) -> Int(a / b)
			|	(Int(a), Mod,  Int(b)) -> Int(a mod b)
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
	| MapCall(someMapper,argList) -> (match someMapper with 
			AnonyMap(stList) -> run_mapper stList argList env
		|	NamedMap(mapname) -> run_named_mapper mapname argList env
		)
	| _ -> Void
		

and function_call fname fargs env = match (fname, fargs) with
		("pattern", []) -> Pattern([])
	|	("pattern", [arg]) -> let v = evaluate arg env in
			(
				match v with
				Str(x) ->
				(
					let charlist = Str.split (Str.regexp "") x
					in let revlist =
							List.fold_left
							(
								fun bl str ->
								(
									match str with
									  "0" -> false
									| "1" -> true
									| _   -> raise (PatternParse_error "Patterns definitions must be a string of 0's and 1's")
								) :: bl
							)
							[] charlist
						in Pattern(List.rev revlist)
				)
				| _ -> raise (Type_error "Pattern definitions take a string argument")
			)
	|	("print", []) -> print_endline ""; Void
	|	("print", [arg]) -> let v = evaluate arg env in
			(
				match v with
				  Str(x)  -> print_endline x; Void
				| Int(y)  -> print_endline(string_of_int y); Void
				| Bool(z) -> print_endline(if z then "TRUE" else "FALSE"); Void
				| Pattern(p) ->
					List.iter (fun x -> print_string (if x then "1" else "0")) p;
					print_string "\n";
					Void
				| Beat(a,i) -> let state = state_of_beat (a,i) in print_endline(
						match state with None->"NULL" | Some(b) -> if b then "YES" else "NO"
					); Void
				| _ -> print_endline("Dunno how to print this yet."); Void
			)
	|	("concat", []) -> Pattern([])
	|	("concat", head::tail) -> let headVal = evaluate head env in
			let tailVal = function_call "concat" tail env in
		(
			match (headVal, tailVal) with
				(Pattern(headP), Pattern(tailP)) -> Pattern(headP @ tailP)
			|	(_, _)                           -> raise (Invalid_argument "concat can inly be used on patterns")
		)
	|	(other, _)	-> (* TODO: currently also catches invalid argument-counts,
							which should probably be intercepted further up the line *)
			let msg =  "Function name '" ^ other ^ "' is not a valid function." in
				raise (Invalid_function msg)

and member_call objectExpr mname margs env = let objectVal = evaluate objectExpr env in
	match (objectVal, mname, margs) with
	(Pattern(x), "repeat", margs) ->
	(
		match margs with
			[argExpr] -> let argVal = evaluate argExpr env in
			(
				match argVal with
				  Int(y) -> if (y < 0) then raise (Invalid_argument "Repeat can only accept non-negative integers")
							else if (y == 0) then Pattern([])
							else let rec repeatPattern p n = if n == 1 then p else p @ repeatPattern p (n-1)
								in Pattern(repeatPattern x y)
				| _ -> raise (Invalid_function "Member function repeat expects an integer argument")
			)
			| _ -> raise (Invalid_function "Member function repeat expects a single arguments")
	)

	|	(Pattern(x), "length", margs) ->
		(
			 match margs with
					[]  ->  Int(List.length x)
				|	_   -> raise (Invalid_function "Member function length expects no arguments")
		)
	|	(Pattern(x), "slice", [startExpr; lenExpr]) -> let startVal = evaluate startExpr env in
													   let lenVal   = evaluate lenExpr   env in
			(
				match (startVal, lenVal) with
				(Int(s), Int(l)) ->    if s < 1 || (s > List.length x && List.length x > 0) then raise (Invalid_argument "the start position is out of bounds")
									else if l < 0  then raise (Invalid_argument "the length must be non-negative") 
									else  let rec subList inList i minPos maxPos = match inList with
										  []         -> []
										| head::tail -> if      i < minPos then subList tail (i+1) minPos maxPos
														else if i = maxPos then [head]
														else if i > maxPos then []
														else                    head :: (subList tail (i+1) minPos maxPos)
									in Pattern(subList x 1 s (s+l-1))
				| (_, _) -> raise (Invalid_argument "slice must be given integers values for the start position and length")
			)
	| (Pattern(x), "tbm", margs) -> print_endline "Thierry rulzzzzzzzzzz!!!!" ;Void;
	| _ -> raise (Invalid_function "Undefined member function")

and execute s env = match s with
		Expr(e) -> ignore(evaluate e env); env
	| IfBlock(tExpr,iftrue,iffalse) -> let tVal = evaluate tExpr env
		in (match tVal with
				Bool(true) -> execlist iftrue env
			| 	Bool(false) -> (match iffalse with
					Some(stlist) -> execlist stlist env
				|	None -> env
				)
			|	_	-> raise ( Type_error "test of if block must be a boolean")
		)
	| Assign(varName,valExpr) ->
		let valVal = evaluate valExpr env in
		(match valVal with 
		    Bool(x) -> raise(Illegal_assignment "do you try to assign a boolean? 20$ and I don't tell")
		  | Str(x) -> raise(Illegal_assignment "do you try to assign a string? pfffff....")
                  | Beat(x,y) -> raise(Illegal_assignment "do you try to assign a beat? you s*ck!")
                  | PatternAlias(x) -> raise(Illegal_assignment "do you try to assign a patternalias? alright, I don't even know what it is")
		  | _ ->
		      let symbolTable = env.symbols in
			(* XXX mask variables in outer scope?  Or error? *)
			{symbols = NameMap.add varName valVal symbolTable; parent=env.parent}
		)
	| MapDef(mapname, formal_params, contents) ->
		if (NameMap.mem mapname env.symbols) then raise (Failure"don't do that")
		else
			let newMapper = Mapper(mapname,formal_params,contents) in
			let newST = NameMap.add mapname newMapper env.symbols  in
			{symbols = newST;parent=env.parent}
	| Return(retExpr) -> 
		(match env.parent with 
				None -> raise(Failure "no you don't")
			| 	_ -> if (not (NameMap.mem "return" env.symbols)) then
					raise (Failure "still don't")
				else
				let retVal = evaluate retExpr env in 
				let newenv = add_key_to_env env "return" retVal in
				raise (Return_value newenv)
		)
	| EmptyStat -> env

and execlist slist env =
	List.fold_left (fun env s -> execute s env) env slist
and execlist_returning slist env =
	try List.fold_left (fun env s -> execute s env) env slist
	with 
			Return_value(newenv) -> newenv
		| 	other -> raise other

let run p env = match p with
	Content(statements) -> ignore(execlist statements env)

let _ =
let unscoped_env = {symbols = NameMap.empty; parent= None} in
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
ignore (run programAst unscoped_env);;

