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


(* default instruments *)
let default_instr = ["hihat";"snare";"kick"];


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

(* exception used to handle return statement, similar to MicroC from Edwards *)
exception Return_value of drul_env



(* create an empty clip of given size (an array of empty patterns) 
assumes non empty list (clipLen > 0)
*)
let emptyClip clipSize = 
  let rec emptyPatternList len =
    if len == 1 then [[]]
    else (List.append [[]] (emptyPatternList (len - 1)))
  in  Array.of_list (emptyPatternList clipSize)


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

(*
	Adds a given key & value to env in (env, parentEnv).
	Returns the modified env.
*)
let add_key_to_env env key value =
	match env with {symbols=old_st; parent=whatever} ->
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


let string_of_pattern p = 
	List.fold_left (fun a x -> a ^ (if x then "1" else "0")) "" p

let state_of_beat beat =
	match beat with Beat(pattern_data,idx) ->
		let pattern_length = Array.length pattern_data in
		if (idx < 0 or idx >= pattern_length) then None else Some(pattern_data.(idx))
	| _ -> raise (Failure "How did you even get here?")

(* find the position of an instrument in the instruments in the env, returns -1 if doesn't find it *)
let get_instrument_pos env instrName =
	try
		let instrListDrul = get_key_from_env env "instruments" in
		match instrListDrul with
			Instruments(instrList) ->
			  let rec find_pos strList counter  =
			(
			  match strList with
				  []           -> -1
				|	head::tail -> if (String.compare head instrName) == 0
							  then counter
							  else find_pos tail (counter + 1)
			)
			  in find_pos instrList 0
		  | _ -> raise (Failure "weird stuff in env for instruments...")
	with Undefined_identifier(e) -> raise (Failure "instrument not saved in env yet")
	  | Failure(e) -> raise (Failure e)
	  | _ -> raise (Failure "wrong exception in get_instrument_pos")

let rec concat_pattern_list plist =
	match plist with 
		[]	-> []
	|	Pattern(x)::tail -> x @ (concat_pattern_list tail)
	| 	_ -> raise (Invalid_argument "concat only concatenates patterns")

let rec fill_in_clip_patterns empty_clip pattern_list idx = match pattern_list with 
		[]	-> Clip(empty_clip) (* not technically empty any more *)
			(* TODO: catch array out of bounds here *)
	|	Pattern(p)::tail -> ignore(empty_clip.(idx) <- p); fill_in_clip_patterns empty_clip tail (idx + 1)
	|	InstrumentAssignment(_,_)::tail -> raise (Invalid_argument "clip arguments may not mix styles")
	| 	_	-> raise (Invalid_argument "clip arguments must all evaluate to patterns")

let rec fill_in_clip_instr_assigns empty_clip assignment_list env = match assignment_list with 
		[]	-> Clip(empty_clip) (* not technically empty any more *)
	|	InstrumentAssignment(instrName,p)::tail ->
			(* TODO catch possible exception from incorrect instrument name *)
			let idx = get_instrument_pos env instrName  in
			ignore(empty_clip.(idx) <- p); fill_in_clip_instr_assigns empty_clip tail env 
	|	Pattern(_)::tail ->raise (Invalid_argument "clip arguments may not mix styles")
	| 	_	-> raise (Invalid_argument "clip arguments must all evaluate to instrument assignments")


let make_clip argVals env = 
	let instrument_list = get_key_from_env env "instruments"  (* XXX probably worth trapping *) in
	let num_instrs = (match instrument_list with Instruments(i) -> List.length i 
										| _ ->raise (Failure "can't happen")) in
	let new_clip = emptyClip num_instrs in
	let first_arg = List.hd argVals in
	match first_arg with 
Pattern(_) -> fill_in_clip_patterns new_clip argVals 0
		|	InstrumentAssignment(_,_) ->fill_in_clip_instr_assigns new_clip argVals env
		|	_ -> raise (Invalid_argument "clip arguments must be patterns or instrument assignments")

	
	
	

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
	
and output_call outname outargs env = 
		match (outname , outargs) with 
			  ("txtfile_truncate",[firstArg;secondArg]) -> output_func firstArg secondArg 0 env 
		|	  ("txtfile_append",[firstArg;secondArg])   -> output_func firstArg secondArg 1 env 
		| 	  ( _ , _ )	->	raise (Invalid_function "Usage: output.txtfile_option(filename,stuff to write to the file)")
				   
and output_func firstArg secondArg flag env = 
		let firstExpr = evaluate firstArg env in
		let secondExpr = evaluate secondArg env in				
			match firstExpr with 
				Str(x) -> 
						if(String.length x < 1)then raise (Invalid_argument "File Name needs to be atleast of length 1.")											
						else(
							let fd =	if(flag == 0)then (open_out_gen [Open_creat ; Open_trunc ; Open_wronly] 666 x)
									 	else (open_out_gen [Open_creat ; Open_append] 666 x)
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
		let valVal = evaluate valExpr env in
		(match valVal with
			Bool(x) -> raise(Illegal_assignment "do you try to assign a boolean? 20$ and I don't tell")
		  | Str(x) -> raise(Illegal_assignment "do you try to assign a string? pfffff....")
				  | Beat(x,y) -> raise(Illegal_assignment "do you try to assign a beat? you s*ck!")
				  | PatternAlias(x) -> raise(Illegal_assignment "do you try to assign a patternalias? alright, I don't even know what it is")
		  | _ ->
			(* Does in fact mask variables in outer scope! Not an error! *)
			add_key_to_env env varName valVal
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

let run p env = match p with
	Content(statements) -> ignore(execlist statements env)

let _ =
let unscoped_env = {symbols = NameMap.empty; parent= None} in
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in
Random.self_init (); ignore (run programAst unscoped_env);;

