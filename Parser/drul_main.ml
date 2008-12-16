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
let default_instr = ["hihat";"snare";"kick"];

(* exception used to handle return statement, similar to MicroC from Edwards *)
exception Return_value of drul_env
	
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
	Mapper(mapname2,a_list,stat_list) ->
	  (* check if we receive the good number of patterns *)
	  if List.length a_list != List.length argList  then raise (Invalid_argument "wrong number of inputs for named mapper")
	  else if String.compare mapname mapname2 != 0 then raise (Failure "intern mapper name problem")
	  else run_mapper stat_list argList env a_list
	  (* if given name is not bound to a mapper, Type_error *)
	  | _ -> raise (Type_error "we were expecting a mapper, name associated with something else")

(* main function of a map, takes a list of statement (body of the mapper)
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
	| 	headExp::tail -> (
			let headVal =  evaluate headExp env
			in headVal :: (eval_arg_list tail env)
		)

(* evaluate expressions, no modifications to the environment! *)
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
	|	MapCall(someMapper,argList) ->
		(
			match someMapper with
				AnonyMap(stList) -> run_mapper stList argList env []
			|	NamedMap(mapname) -> run_named_mapper mapname argList env
		)
	|	InstrAssign(instName, patExpr) -> let patVal = evaluate patExpr env in
		(
			match patVal with
				Pattern(p) -> InstrumentAssignment(instName, p)
			|	_ -> raise (Invalid_argument "Only patterns can be assigned to instruments")
		)
	| Output(firstExpr, argList) -> output_call firstExpr argList env

	
(* Used when there is an output.txtfile_option call. *)	
and output_call outname outargs env = 
		match (outname , outargs) with 
			  ("txtfile_truncate",[firstArg;secondArg]) -> output_func firstArg secondArg 0 env 
		|	  ("txtfile_append",[firstArg;secondArg])   -> output_func firstArg secondArg 1 env 
		| 	  ( _ , _ )	->	raise (Invalid_function "Usage: output.txtfile_option(filename,stuff to write to the file)")
	
(* Takes the two arguments of output.txtfile call and puts the second
   argument in the file with the same name as the first argument of output.txtfile****)	
and output_func firstArg secondArg flag env = 
	let firstExpr = evaluate firstArg env in
	let secondExpr = evaluate secondArg env in				
		match firstExpr with 
			Str(x) -> 
				if(String.length x < 1)then raise (Invalid_argument "File Name needs to be atleast of length 1.")											
				else(
					let fd =	if(flag == 0)then (open_out_gen [Open_creat ; Open_trunc ; Open_wronly] 511 x)
								else (open_out_gen [Open_creat ; Open_append] 511 x)
					in
					match secondExpr with
						Str(y) ->	output_string fd y ; flush fd ; output_string fd "\n"; close_out_noerr fd; Void
					|	Int(y)  -> 	output_string fd (string_of_int y) ; flush fd ; output_string fd "\n";close_out_noerr fd; Void 
					| 	Bool(z) -> 
							(
									 if z then output_string fd "TRUE"
									else output_string fd "FALSE" ; 
									flush fd ; output_string fd "\n";	close_out_noerr fd; Void
							)
					| 	Beat(_,_) ->
							(
									let state = state_of_beat secondExpr in
									output_string fd (match state with None->"NULL" | Some(b) -> if b then "NOTE" else "REST");
									flush fd ; output_string fd "\n";	close_out_noerr fd; Void
							)
					|	Pattern(p) ->
							(
								let pstr = 	string_of_pattern p in 
											output_string fd pstr ; flush fd ; output_string fd "\n";	close_out_noerr fd; Void
							)
					| 	Clip(ar) -> let instrVal = get_key_from_env env "instruments" in
									let instr_names = (match instrVal with Instruments(l) -> l | _ ->raise (Failure "can't happen")) in
									output_string fd "[";output_string fd "\n";
									ignore(List.fold_left 
										(fun i name -> 
											output_string fd ("\t" ^ name ^ ":\t" ^ (string_of_pattern ar.(i))); output_string fd "\n";(i+1))
										0
										instr_names
										);
									output_string fd "]";output_string fd "\n";Void		

					|	_			->	raise (Invalid_argument "You can't output this to a file.")
					)
				
			| 	_ 	-> raise (Invalid_argument "output.txtfile_option accepts a string stating the file name") 
		

(* handle the general case of a.b() *)
and function_call fname fargs env = 
	let fargvals = eval_arg_list fargs env in
	match (fname, fargvals) with
		("pattern", []) -> Pattern([])
	|	("pattern", [v]) -> (match v with
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
	|	("print", [v]) -> (
				match v with
				  Str(x)  -> print_endline x; Void
				| Int(y)  -> print_endline(string_of_int y); Void
				| Bool(z) -> print_endline(if z then "TRUE" else "FALSE"); Void
				| Pattern(p) -> let pstr = string_of_pattern p in print_endline pstr;Void
				| Beat(_,_) -> let state = state_of_beat v in print_endline(
						match state with None->"NULL" | Some(b) -> if b then "NOTE" else "REST"
					); Void
				| Clip(ar) -> let instrVal = get_key_from_env env "instruments" in
					let instr_names = (match instrVal with Instruments(l) -> l | _ ->raise (Failure "can't happen")) in
					print_endline "[";
					ignore(List.fold_left 
						(fun i name -> print_endline ("\t" ^ name ^ ":\t" ^ (string_of_pattern ar.(i))); (i+1))
						0
						instr_names
					);
					print_endline "]";
					Void						
				| _ -> print_endline("Dunno how to print this yet."); Void
			)
	|	("concat", concat_args) -> let catenated = concat_pattern_list concat_args in Pattern(catenated)
	|	("rand", []) -> Int(Random.int 2)
	|	("rand", [argVal]) -> (
				match argVal with
				Int(bound) -> if bound > 0 then Int(Random.int bound)
							  else raise (Invalid_argument "the rand function expects a positive integer argument")
				| _ -> raise (Invalid_argument "the rand function expects an integer argument")
			)
	|	("rand", _) -> raise (Invalid_argument "the rand function expects a single, optional, positive, integer argument")
	|	("clip", argList) -> make_clip argList env
	|	(other, _)	-> (* TODO: currently also catches invalid argument-counts,
							which should probably be intercepted further up the line *)
			let msg =  "Function name '" ^ other ^ "' is not a valid function." in
				raise (Invalid_function msg)

				
(* Method Calls *)
and member_call objectExpr mname margs env = 
	let objectVal = evaluate objectExpr env in
	let argVals	  = eval_arg_list margs env in
	match (objectVal, mname, argVals) with
	(Pattern(x), "repeat", margs) ->
	(
		match margs with
			[argVal] -> (
				match argVal with
				  Int(y) -> if (y < 0) then raise (Invalid_argument "Repeat can only accept non-negative integers")
							else if (y == 0) then Pattern([])
							else let rec repeatPattern p n = if n == 1 then p else p @ repeatPattern p (n-1)
								in Pattern(repeatPattern x y)
				| _ -> raise (Invalid_function "Member function repeat expects an integer argument")
			)
			| _ -> raise (Invalid_function "Member function repeat expects a single argument")
	)

	|	(Pattern(x), "length", margs) ->
		(
			 match margs with
					[]  ->  Int(List.length x)
				|	_   -> raise (Invalid_function "Member function length expects no arguments")
		)
	|	(Pattern(x), "slice", [startVal; lenVal]) -> (
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
	| (Beat(a,i), "note", []) -> let beatval = state_of_beat objectVal in
		( match beatval with Some(yesno) -> Bool(yesno) | None -> Bool(false) )
	| (Beat(a,i), "rest", []) -> let beatval = state_of_beat objectVal in
		( match beatval with Some(yesno) -> Bool(not yesno) | None -> Bool(false) )
	| (Beat(a,i), "prev", [offsetVal]) -> (match offsetVal with
			Int(offsetInt) -> let newidx = i - offsetInt in Beat(a,newidx)
			| _ -> raise (Invalid_function "Beat method 'prev' requires an integer argument")
		)
	| (Beat(a,i), "next", [offsetVal]) -> (match offsetVal with
			Int(offsetInt) -> let newidx = i + offsetInt in Beat(a,newidx)
			| _ -> raise (Invalid_function "Beat method 'next' requires an integer argument")
		)
	| _ -> raise (Invalid_function "Undefined member function")


(* similar to evaluate, but handles cases like assignment, where the environment is modified *)
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
	    (match varName with
		"pattern" -> raise(Illegal_assignment "can't assign to 'pattern'")
	      | "rand" -> raise(Illegal_assignment "can't assign to 'rand'")
	      | "clip" -> raise(Illegal_assignment "can't assign to 'clip'")
	      | _ -> let valVal = evaluate valExpr env in
			(match valVal with
				Bool(x) -> raise(Illegal_assignment "do you try to assign a boolean? 20$ and I don't tell")
			| Str(x) -> raise(Illegal_assignment "do you try to assign a string? pfffff....")
			| Beat(x,y) -> raise(Illegal_assignment "do you try to assign a beat? you s*ck!")
			| PatternAlias(x) -> raise(Illegal_assignment "do you try to assign a patternalias? alright, I don't even know what it is")
			| _ ->
				(* Does in fact mask variables in outer scope! Not an error! *)
				add_key_to_env env varName valVal
			)
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
	| InstrDef(argList) ->
		(try

		   ignore(get_key_from_env env "instruments");
		   raise (Instruments_redefined "don't do that")
		 with
			 Undefined_identifier e -> let strList = eval_arg_list argList env in
			 let str_to_string a =
			(
			  match a with
				  Str(s) -> s
				| _ -> raise (Invalid_argument "instruments takes a list of strings")
			) in
			 let stringList = List.map str_to_string strList in
			(
			  match stringList with
				  [] -> (* default *)
					add_key_to_env env "instruments" (Instruments(default_instr))
				| _ -> let instVal = Instruments(stringList) in
				add_key_to_env env "instruments" instVal
			)
		   | Instruments_redefined(e) -> raise (Instruments_redefined e)
		   | _ -> raise (Failure "Unexpected exception during instrument definition")
		)
	| EmptyStat -> env

and execlist slist env =
	List.fold_left (fun env s -> execute s env) env slist
	
and execlist_returning slist env =
	try List.fold_left (fun env s -> execute s env) env slist
	with
			Return_value(newenv) -> newenv
		| 	other -> raise other
