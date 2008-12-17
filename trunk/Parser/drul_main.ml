(*
***********************************************************************
*      			 DruL - Drumming Language
*
* Creation of R. Stewart, T. Bertin-Mahieux, W. Ilahi  and B. Warfield
*             rs2660      tb2332             wki2001       bbw2108
*
* for the class COMS W4115: Programming Language and Translators
*
* file: drul_main.ml
*
*       MAIN
*
* This file contains the main driver functions for the DruL interpreter.
* This code is written in OCaml.
*
*************************************************************************
*)

open Drul_ast
open Drul_types
open Drul_helpers

(* default instruments *)
let default_instr = ["hihat"; "snare"; "kick"];

(* exception used to handle return statement, similar to MicroC from Edwards *)
exception Return_value of drul_env

(*
	inside a map, do one step!
	return is saved as "return" in the env
	current index is saved as "$current" in the env
*)
let rec one_mapper_step maxiters current st_list env current_pattern =
	if (maxiters == current) then Pattern(current_pattern)
	else
		let retval = Pattern([]) in
		let env = add_key_to_env env "return" retval in
		let env = add_key_to_env env "$current" (Int(current)) in
		let newenv = execlist_returning st_list env in
		let new_st = newenv.symbols in
		let return = NameMap.find "return" new_st in
		let current_pattern =
		(
			match return with
				Pattern(bools) -> current_pattern @ bools
			|	_ -> (raise (Failure "in one_mapper_step, should not happen"))
		)
		in
		let current = current + 1 in
		one_mapper_step maxiters current st_list newenv current_pattern

(*
	run a named mapper,
	find the mapper in the env,
	and cast it to a anonymous mapper
*)
and run_named_mapper mapname argList env lineno =
  let savedmapper = get_key_from_env env mapname lineno in
	match savedmapper with
	Mapper(mapname2,a_list,stat_list) ->
	  (* check if we receive the good number of patterns *)
	  if List.length a_list != List.length argList then raise (Invalid_argument ("wrong number of inputs for named mapper", lineno))
	  else if String.compare mapname mapname2 != 0 then raise (Failure "in run_named_mapper, should not happen (intern mapper name problem)")
	  else run_mapper stat_list argList env a_list
	  (* if given name is not bound to a mapper, Type_error *)
	  | _ -> raise (Type_error ("we were expecting a mapper, name associated with something else", lineno))

(*
	main function of a map, takes a list of statement (body of the mapper)
	evaluate the arg_list, which should be a list of patterns
	launches the iteration (one_mapper_step)
*)
and run_mapper statement_list arg_list env a_list =
	let arg_list_evaled = eval_arg_list arg_list env in
	let map_env = get_map_env env arg_list_evaled a_list in
	let max_iters = find_longest_list arg_list_evaled in
	one_mapper_step max_iters 0 statement_list map_env []

(* evaluate an expr_list when we know that they're all patterns *)
and eval_arg_list arg_list env = match arg_list with
		[] -> []
	| 	headExp::tail ->
		(
			let headVal =  evaluate headExp env
			in headVal :: (eval_arg_list tail env)
		)



(* evaluate expressions, no modifications to the environment! *)
and evaluate e env = match e.real_expr with
		FunCall(fname, fargs) -> function_call fname fargs env e.lineno
	|	MethodCall(objectExpr, mname, margs) -> method_call objectExpr mname margs env
	|	CStr (x) -> Str (x)
	|   CBool(x) -> Bool(x)
	|	CInt (x) -> Int (x)
	|	Var(name) -> let fetched = get_key_from_env env name e.lineno in		(
			match fetched with
				PatternAlias(alias) -> beat_of_alias env alias e.lineno
			|	other -> other
		)
	|	UnaryMinus(xE) -> let xV = evaluate xE env in
		(
			match xV with
				Int(x) -> Int(-x)
			|	_      -> raise (Type_error ("you can't negate that, dorkface", e.lineno))
		)
	|	UnaryNot(xE) ->  let xV = evaluate xE env in
		(
			match xV with
				Bool(x) -> Bool(not x)
			|	_       -> raise (Type_error ("you can't contradict that", e.lineno))
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
			|	_ -> raise (Type_error ("cannot do arithmetic on non-integers", e.lineno))
		)
	|	LogicBinop(aExp, operator, bExp) ->
		let aVal = evaluate aExp env in
		let bVal = evaluate bExp env in
		(
			match (aVal, operator, bVal) with
				(Bool(x), And, Bool(y)) -> Bool(x && y)
			|	(Bool(x), Or,  Bool(y)) -> Bool(x || y)
			|	_ -> raise (Type_error ("cannot do logical operations except on booleans", e.lineno))
		)
	|	Comparison(aExp, operator, bExp) ->
		let aVal = evaluate aExp env in
		let bVal = evaluate bExp env in
		(
			match (aVal, operator, bVal) with
				(Int(a), LessThan,    Int(b)) -> Bool(a <  b)
			|	(Int(a), GreaterThan, Int(b)) -> Bool(a >  b)
			|	(Int(a), LessEq,      Int(b)) -> Bool(a <= b)
			|	(Int(a), GreaterEq,   Int(b)) -> Bool(a >= b)
			|	(Int(a), EqualTo,     Int(b)) -> Bool(a == b)
			|	(Int(a), NotEqual,    Int(b)) -> Bool(a != b)
			|	_ -> raise (Type_error ("cannot do that comparison operation", e.lineno))
		)
	|	MapCall(someMapper,argList) ->
		(
			match someMapper with
				AnonyMap(stList)  -> run_mapper stList argList env []
			|	NamedMap(mapname) -> run_named_mapper mapname argList env e.lineno
		)
	|	InstrAssign(instName, patExpr) -> let patVal = evaluate patExpr env in
		(
			match patVal with
				Pattern(p) -> InstrumentAssignment(instName, p)
			|	_          -> raise (Invalid_argument ("Only patterns can be assigned to instruments", e.lineno))
		)



(* Used when there is an output.txtfile_option call. *)
(*
and output_call outname outargs env lineno =
	match (outname , outargs) with
		("txtfile_truncate", [firstArg; secondArg]) -> output_func firstArg secondArg 0 env lineno
	|	("txtfile_append",   [firstArg; secondArg]) -> output_func firstArg secondArg 1 env lineno
	|	(_ ,_)	->	raise (Invalid_function ("Usage: output.txtfile_option(filename,stuff to write to the file)", lineno))
*)

(* Takes the two arguments of output.txtfile call and puts the second
   argument in the file with the same name as the first argument of output.txtfile
*)
(*
and output_func firstArg secondArg flag env lineno =
	let firstExpr  = evaluate firstArg  env in
	let secondExpr = evaluate secondArg env in
		match firstExpr with
			Str(x) ->
				if(String.length x < 1)then raise (Invalid_argument ("File Name needs to be atleast of length 1.", lineno))
				else(
					let fd =	if(flag == 0)then (open_out_gen [Open_creat ; Open_trunc ; Open_wronly] 511 x)
								else (open_out_gen [Open_creat ; Open_append] 511 x)
					in

					| 	Clip(ar) -> let instrVal = (get_key_from_env env "instruments" (-1)) in
									let instr_names = (match instrVal with Instruments(l) -> l | _ ->raise (Failure "can't happen")) in
									output_string fd "[";output_string fd "\n";
									ignore(List.fold_left
										(fun i name ->
											output_string fd ("\t" ^ name ^ ":\t" ^ (string_of_pattern ar.(i))); output_string fd "\n";(i+1))
										0
										instr_names
										);
									output_string fd "]";output_string fd "\n";Void

					|	_			->	raise (Invalid_argument ("You can't output this to a file.", lineno))
					)
			| 	_ 	-> raise (Invalid_argument ("output.txtfile_option accepts a string stating the file name", lineno))
*)


(* 
function calls, anything looking like a() or a(something) 
the major 'match' is done on a
*)
and function_call fname fargs env lineno =
	let fargvals = eval_arg_list fargs env in
	match (fname, fargvals) with
		("pattern", []) -> Pattern([])
	|	("pattern", [v]) ->
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
								|	"1" -> true
								|	_   -> raise (PatternParse_error ("Patterns definitions must be a string of 0's and 1's", lineno))
							) :: bl
						)
						[] charlist
						in Pattern(List.rev revlist)
				)
			|	_ -> raise (Type_error ("Pattern definitions take a string argument",lineno))
		)
	|	("print", []) -> print_endline ""; Void
	|	("print", [v]) ->
		(
			match v with
				Str(x)     -> print_endline x; Void
			|	Int(y)     -> print_endline(string_of_int y); Void
			|	Bool(z)    -> print_endline(if z then "TRUE" else "FALSE"); Void
			|	Pattern(p) -> let pstr = string_of_pattern p in print_endline pstr;Void
			|	Beat(_,_)  -> let state = state_of_beat v in print_endline
				(
					match state with
						None    -> "NULL"
					|	Some(b) -> if b then "NOTE" else "REST"
				); Void
			|	Clip(ar) -> let instrVal = get_key_from_env env "instruments" lineno in
					let instr_names =
					(
						match instrVal with
							Instruments(l) -> l
						|	_ -> raise (Failure "in gunction_call, case (print,[v]), should not happen")
					) in
					print_endline "[";
					ignore
					(
						List.fold_left
						(fun i name -> print_endline ("\t" ^ name ^ ":\t" ^ (string_of_pattern ar.(i))); (i+1))
						0
						instr_names
					);
					print_endline "]";
					Void
				| _ -> print_endline("Dunno how to print this yet."); Void
			)
	|	("concat", concat_args) -> let catenated = concat_pattern_list concat_args lineno in Pattern(catenated)
	|	("rand", []) -> Int(Random.int 2)
	|	("rand", [argVal]) ->
		(
			match argVal with
				Int(bound) -> if bound > 0 then Int(Random.int bound)
							  else raise (Invalid_argument ("the rand function expects a positive integer argument", lineno))
			|	_ -> raise (Invalid_argument ("the rand function expects an integer argument", lineno))
		)
	|	("rand", _) -> raise (Invalid_argument ("the rand function expects a single, optional, positive, integer argument", lineno))
	|	("clip", argList) -> make_clip argList env lineno
	|	(other, _) -> (* TODO: currently also catches invalid argument-counts, which should probably be intercepted further up the line *)
			let msg = "Function name '" ^ other ^ "' is not a valid function." in
				raise (Invalid_function (msg, lineno))



(* 
Method Calls, anything looking like a.b() or a.b(something) 
the major 'match' is usually done on both a and b
*)
and method_call objectExpr mname margs env =
	let objectVal = evaluate objectExpr env in
	let argVals	  = eval_arg_list margs env in
	match (objectVal, mname, argVals) with
		(Pattern(x), "repeat", margs) ->
		(
			match margs with
				[argVal] ->
				(
					match argVal with
					Int(y) -> if      (y  < 0) then raise (Invalid_argument ("Repeat can only accept non-negative integers", objectExpr.lineno))
							  else if (y == 0) then Pattern([])
							  else let rec repeatPattern p n = if n == 1 then p else p @ repeatPattern p (n-1)
							  in Pattern(repeatPattern x y)
					| _ -> raise (Invalid_function ("Method function repeat expects an integer argument", objectExpr.lineno))
				)
			|	_ -> raise (Invalid_function ("Method function repeat expects a single argument", objectExpr.lineno))
		)
	|	(Pattern(x), "length", margs) ->
		(
			 match margs with
				[]  ->  Int(List.length x)
			|	_   -> raise (Invalid_function ("Method function length expects no arguments",objectExpr.lineno))
		)
	|	(Pattern(x), "slice", [startVal; lenVal]) ->
		(
			match (startVal, lenVal) with
				(Int(s), Int(l)) -> if s < 1 || (s > List.length x && List.length x > 0) then raise (Invalid_argument("the start position is out of bounds", objectExpr.lineno))
									else if l < 0  then raise (Invalid_argument ("the length must be non-negative",objectExpr.lineno))
									else  let rec subList inList i minPos maxPos =
									(
										match inList with
										  []         -> []
										| head::tail -> if      i < minPos then subList tail (i+1) minPos maxPos
														else if i = maxPos then [head]
														else if i > maxPos then []
														else                    head :: (subList tail (i+1) minPos maxPos)
									)
									in Pattern(subList x 1 s (s+l-1))
			|	(_, _) -> raise (Invalid_argument ("slice must be given integers values for the start position and length", objectExpr.lineno))
		)
	|	(Beat(a,i), "note", []) -> let beatval = state_of_beat objectVal in
		(
			match beatval with
				Some(yesno) -> Bool(yesno)
			|	None        -> Bool(false)
		)
	|	(Beat(a,i), "rest", []) -> let beatval = state_of_beat objectVal in
		(
			match beatval with
				Some(yesno) -> Bool(not yesno)
			|	None        -> Bool(false)
		)
	|	(Beat(a,i), "prev", [offsetVal]) ->
		(
			match offsetVal with
				Int(offsetInt) -> let newidx = i - offsetInt in Beat(a,newidx)
			|	_              -> raise (Invalid_function ("Beat method 'prev' requires an integer argument", objectExpr.lineno))
		)
	|	(Beat(a,i), "next", [offsetVal]) ->
		(
			match offsetVal with
				Int(offsetInt) -> let newidx = i + offsetInt in Beat(a,newidx)
			|	_              -> raise (Invalid_function ("Beat method 'next' requires an integer argument", objectExpr.lineno))
		)
	|	(Clip(ar), "outputText", [fileVal]) ->
		(
			match fileVal with
			Str(fileName) ->
				if (String.length fileName) < 1 then raise (Invalid_argument ("Output filename must have 1 or more characters", objectExpr.lineno))
				else let instrVal = get_key_from_env env "instruments" objectExpr.lineno in
					let instr_names =
					(
						match instrVal with
							Instruments(l) -> l
						|	_ -> raise (Failure "can't happen")
					) in
					let out = open_out fileName in
					output_string out "[\n";
					ignore
					(
						List.fold_left
						(fun i name -> output_string out ("\t" ^ name ^ ":\t" ^ (string_of_pattern ar.(i)) ^ "\n"); (i+1))
						0
						instr_names
					);
					output_string out "]\n";
					Void
					| _ -> raise (Invalid_function ("Clip method 'outputText' requires an string argument for the output file", objectExpr.lineno))
		)
	| _ -> raise (Invalid_function ("Undefined method function",objectExpr.lineno))



(* similar to evaluate, but handles cases like assignment, where the environment is modified *)
and execute s env = match s with
	Expr(e) -> ignore(evaluate e env); env
|	IfBlock(tExpr, iftrue, iffalse) -> let tVal = evaluate tExpr env in
	(
		match tVal with
			Bool(true)  -> execlist iftrue env
		| 	Bool(false) ->
			(
				match iffalse with
					Some(stlist) -> execlist stlist env
				|	None         -> env
			)
		|	_ -> raise (Type_error ("test of if block must be a boolean", tExpr.lineno))
	)
|	Assign(varName,valExpr, lineno) ->
	(
		match varName with
			"pattern" -> raise(Illegal_assignment ("can't assign to 'pattern'", lineno))
		|	"rand"    -> raise(Illegal_assignment ("can't assign to 'rand'",    lineno))
		|	"clip"    -> raise(Illegal_assignment ("can't assign to 'clip'",    lineno))
		|	_         -> let valVal = evaluate valExpr env in
			(
				match valVal with
					Bool(x)         -> raise(Illegal_assignment ("can't assign a boolean", lineno))
				|	Str(x)          -> raise(Illegal_assignment ("can't assign a string", lineno))
				|	Beat(x,y)       -> raise(Illegal_assignment ("can't assigna beat", lineno))
				|	PatternAlias(x) -> raise(Illegal_assignment ("can't assign a PatternAlias, whatever it is", lineno))
				| _ -> add_key_to_env env varName valVal (* Does in fact mask variables in outer scope! Not an error! *)
			)
	)
|	MapDef(mapname, formal_params, contents, lineno) ->
		if (NameMap.mem mapname env.symbols) then raise (Failure"don't do that")
		else
			let newMapper = Mapper(mapname,formal_params,contents) in
			let newST = NameMap.add mapname newMapper env.symbols in
			{symbols = newST; parent = env.parent}
|	Return(retExpr) ->
	(
		match env.parent with
			None -> raise(Failure "in execute, case Return, should not happen (None parent?)")
		| 	_    -> if (not (NameMap.mem "return" env.symbols)) then raise (Failure "still don't")
					else
					let retVal = evaluate retExpr env in
					let newenv = add_key_to_env env "return" retVal in
					raise (Return_value newenv)
	)
|	InstrDef(argList, lineno) ->
	(
		try
			ignore(get_key_from_env env "instruments" lineno);
			raise (Instruments_redefined ("don't do that", lineno)) (*XXX could be improved...*)
		with
			Undefined_identifier (_,_) -> let strList = eval_arg_list argList env in
				let str_to_string a =
				(
					match a with
						Str(s) -> s
					|	_      -> raise (Invalid_argument ("instruments takes a list of strings", lineno))
				) in
				let stringList = List.map str_to_string strList in
				(
					match stringList with
						[] -> add_key_to_env env "instruments" (Instruments(default_instr)) (* default *)
					|	_  -> let instVal = Instruments(stringList) in
								add_key_to_env env "instruments" instVal
				)
			| Instruments_redefined(e,i) -> raise (Instruments_redefined (e,i))
			| _ -> raise (Failure "in execute, case InstrDef, unexpected exception")
	)
|	EmptyStat -> env



and execlist slist env = List.fold_left (fun env s -> execute s env) env slist

(* special case used for mapper, when we expect a return value *)
and execlist_returning slist env =
	try List.fold_left (fun env s -> execute s env) env slist
	with
		Return_value(newenv) -> newenv
	| 	other -> raise other
