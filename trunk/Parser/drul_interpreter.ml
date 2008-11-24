open Drul_ast

module NameMap = Map.Make(String)

exception Type_error         of string
exception Invalid_function   of string
exception PatternParse_error of string
exception Invalid_argument   of string
type pattern = bool list

type drul_t = Void
	   | Int     of int
	   | Str     of string
	   | Bool    of bool
	   | Pattern of pattern
	   | Clip    of pattern array
	   | Mapper  of (string * string list * statement list)
	   | BeatAlias of bool array

(* turn a pattern object (list of booleans) into an array, and return
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

(* given a NameMap and a (pattern, alias) pair, add the appropriate
information to the NameMap (at this point, an array of the beats in the pattern) *)
let add_pattern_alias  symbol_table pair =
	let p_list = fst(pair) in
	let alias = snd(pair) in 
	let p_array = Array.of_list p_list in
	let beat_holder = BeatAlias(p_array)
	in NameMap.add alias beat_holder symbol_table
	
(* use the above functions to add the correct entries to a new symbol table
	before entering a "map" block *)
let init_mapper_st p_list a_list =
	let alias_list = get_alias_list p_list a_list 1
	in List.fold_left add_pattern_alias NameMap.empty alias_list

(* create a new symbol table with the appropriate aliases, and link it to the parent *)
let get_map_env parent_env p_list a_list =
	let new_symbol_table = init_mapper_st p_list a_list 
	in (new_symbol_table, Some(parent_env))

let maxlen_helper currmax newlist =
	let currlen = List.length newlist in
	if (currlen > currmax) then currlen else currmax

let find_longest_list patternlist =
	List.fold_left maxlen_helper 0 patternlist
	
let add_key_to_env env key value = 
	match env with (old_st,whatever) -> 
		let new_st = NameMap.add key value old_st
		in (new_st,whatever)

let rec one_mapper_step maxiters current st_list env current_pattern =
	if (maxiters == current) then Pattern(current_pattern)
	else
		let retval = Pattern([]) in 
		let env = add_key_to_env env "return" retval 	in 
		let env = add_key_to_env env "$current" (Int(current)) in 
		let newenv = execlist st_list env in
		let new_st = fst(newenv) in
		let return = NameMap.find "return" new_st in
		let current_pattern = (match return with 
			Pattern(bools) -> current_pattern @ bools 
			| _ -> (raise (Failure "Jerks")) )
		in
		let current = current + 1 in
		one_mapper_step maxiters current st_list newenv current_pattern

and run_mapper statement_list arg_list env = 
	let map_env = get_map_env env arg_list [] in (* FIXME: alias list from mapdef *)
	let max_iters = find_longest_list arg_list in
one_mapper_step max_iters 0 statement_list map_env []


and evaluate e env = match e with
		FunCall(fname, fargs) -> function_call fname fargs env
	|	MemberCall(objectExpr, mname, margs) -> member_call objectExpr mname margs env
	|	CStr (x) -> Str (x)
	|   CBool(x) -> Bool(x)
	|	CInt (x) -> Int (x)
	|	Var(name) -> let symTab = fst(env) in NameMap.find name symTab  (* TODO: Multiple scopes *)
	|	UnaryMinus(xE) -> let xV = evaluate xE env in
		(
			match xV with
				Int(x) -> Int(-x)
			| _ -> raise (Type_error "you can't negate that, dorkface")
		)
		|       UnaryNot(xE) ->  let xV = evaluate xE env in
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
	| MapCall(someMapper,argList) -> match someMapper with 
			AnonyMap(stList) -> run_mapper stList argList env
		|	NamedMap(mapname) -> (raise Failure "Not yet implemented.")
	| _ -> Void

and function_call fname fargs env = match (fname, fargs) with
		("pattern", [arg]) -> let v = evaluate arg env in
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
	|	("pattern", []) -> Pattern([])
	|	("print", [arg]) -> let v = evaluate arg env in
			(
				match v with
				Str(x) -> print_endline x; Void
				| Int(y) -> print_endline (string_of_int y); Void
				| Bool(z) -> print_endline( if z then "TRUE" else "FALSE" ); Void
				| Pattern(p) ->
					List.iter (fun x -> print_string (if (x) then "1" else "0")) p;
					if(List.length p > 0)then print_string "\n";
					Void
				| _ -> print_endline("Dunno how to print this yet."); Void
			)

	|	(other,_)	-> (* TODO: currently also catches invalid argument-counts, which
						  should probably be intercepted further up the line *)
			let msg =  "Function name '"^ other ^"' is not a valid function." in
				raise (Invalid_function msg)

and member_call objectExpr mname margs env = let objectVal = evaluate objectExpr env in
	match (objectVal, mname, margs) with
	(Pattern(x), "repeat", margs) ->
	(
		match margs with
			[argExpr] -> let argVal = evaluate argExpr env in
			(
				match argVal with
				  Int(y) -> if(y <= 0)then raise (Invalid_argument "Repeat can only accept non zero positive integers")
							else let rec repeatPattern p n = if n == 1 then p else p @ repeatPattern p (n-1) in
					  Pattern(repeatPattern x y)
				| _ -> raise (Invalid_function "Member function repeat expects an integer argument")
			)
			| _ -> raise (Invalid_function "Member function repeat expects a single arguments")
	)

	|	(Pattern(x),"length",margs) -> 
		(
			 match margs with
					[]  ->  Int(List.length x)
				|	_   -> raise (Invalid_function "Member function length expects no arguments")   
		)

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
		let symbolTable = fst(env) in
		 (* XXX mask variables in outer scope?  Or error? *)
		 ( (NameMap.add varName valVal symbolTable), snd(env))
	| MapDef(mapname, formal_params, contents) ->
		let symbolTable = fst(env) in
		if (NameMap.mem mapname symbolTable) then raise (Failure"don't do that")
		else
			let newMapper = Mapper(mapname,formal_params,contents) in
			let newST = NameMap.add mapname newMapper symbolTable  in
			(newST,snd(env))
	| _ -> env

and execlist slist env =
	List.fold_left (fun env s -> execute s env) env slist

let run p env = match p with
	Content(statements) -> ignore(execlist statements env)

let _ =
let unscoped_env = NameMap.empty in
let lexbuf = Lexing.from_channel stdin in
let programAst = Drul_parser.program Drul_scanner.token lexbuf in

ignore (run programAst (unscoped_env, None));;

