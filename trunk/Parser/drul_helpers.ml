(*
***********************************************************************
*      			 DruL - Drumming Language
*
* Creation of R. Stewart, T. Bertin-Mahieux, W. Ilahi  and B. Warfield
*             rs2660      tb2332             wki2001       bbw2108
*
* for the class COMS W4115: Programming Language and Translators
*
* file: drul_helpers.ml
*
*       HELPERS
*
* This file contains the helper functions (anything that is not required
* to be mutually recursive with "evaluate") for the DruL interpreter.
* This code is written in OCaml.
*
*************************************************************************
*)

open Drul_ast
open Drul_types



(*
	create an empty clip of given size (an array of empty patterns)
	assumes non empty list (clipLen > 0)
*)
let emptyClip clipSize =
	let rec emptyPatternList len =
		if len == 1 then [[]]
		else (List.append [[]] (emptyPatternList (len - 1)))
	in Array.of_list (emptyPatternList clipSize)



(*
	turn a pattern object (list of booleans) into an array, and return
	pairs of (array, alias) to be added to the symbol table
*)
let rec get_alias_list p_list a_list counter =
	let newcounter = counter + 1 in
		match (p_list,a_list) with
		([],[]) -> []
	|	([],oops) -> raise (Failure "not enough patterns provided to mapper")
	|	(thispat::rest,[]) -> (thispat, "$" ^ string_of_int counter) :: get_alias_list rest [] newcounter
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
	let p_list =
	(
		match p_obj with
			Pattern(pat) -> pat
		|	_            -> raise (Failure "erp")
	) in
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
	Pattern(patlist) ->
	(
		let currlen = List.length patlist in
		if (currlen > currmax) then currlen else currmax
	)
	| _ -> raise (Failure "asshole")

(* find the length of the longest list *)
let find_longest_list patternlist = List.fold_left maxlen_helper 0 patternlist

(*
	Adds a given key & value to env in (env, parentEnv).
	Returns the modified env.
*)
let add_key_to_env env key value =
	match env with {symbols = old_st; parent = whatever} ->
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
		|	None -> raise (Undefined_identifier (key, -1))
and  beat_of_alias env alias =
	let currentVar = get_key_from_env env "$current"
	in match currentVar with
			Int(currentVal) -> Beat(alias,currentVal)
		|	_  -> raise (Failure "Can't have a non-integer in $current--you really can't!!")



let string_of_pattern p = List.fold_left (fun a x -> a ^ (if x then "1" else "0")) "" p

let state_of_beat beat =
	match beat with
		Beat(pattern_data,idx) ->
			let pattern_length = Array.length pattern_data in
			if (idx < 0 or idx >= pattern_length) then None else Some(pattern_data.(idx))
	|	_ -> raise (Failure "How did you even get here?")

(* find the position of an instrument in the instruments in the env, returns -1 if doesn't find it *)
let get_instrument_pos env instrName =
	try
		let instrListDrul = get_key_from_env env "instruments" in
		match instrListDrul with
			Instruments(instrList) ->
				let rec find_pos strList counter  =
				(
					match strList with
						[]          -> -1
					|	head::tail  -> if (String.compare head instrName) == 0 then counter
									   else find_pos tail (counter + 1)
				)
				in find_pos instrList 0
		|	_ -> raise (Failure "weird stuff in env for instruments...")
	with
		Undefined_identifier(e,i) -> raise (Failure "instrument not saved in env yet")
	|	Failure(e)                -> raise (Failure e)
	|	_                         -> raise (Failure "wrong exception in get_instrument_pos")

let rec concat_pattern_list plist =
	match plist with
		[]	-> []
	|	Pattern(x)::tail -> x @ (concat_pattern_list tail)
	| 	_ -> raise (Invalid_argument ("concat only concatenates patterns", -1))



let rec fill_in_clip_patterns empty_clip pattern_list idx lineno = match pattern_list with
		[] -> Clip(empty_clip) (* not technically empty any more *)
			(* TODO: catch array out of bounds here *)
	|	Pattern(p)::tail -> ignore(empty_clip.(idx) <- p); fill_in_clip_patterns empty_clip tail (idx + 1) lineno
	|	InstrumentAssignment(_,_)::tail -> raise (Invalid_argument ("clip arguments may not mix styles", lineno))
	| 	_ -> raise (Invalid_argument ("clip arguments must all evaluate to patterns", lineno))

let rec fill_in_clip_instr_assigns empty_clip assignment_list env lineno = match assignment_list with
		[]	-> Clip(empty_clip) (* not technically empty any more *)
	|	InstrumentAssignment(instrName,p)::tail ->
			(* TODO catch possible exception from incorrect instrument name *)
			let idx = get_instrument_pos env instrName  in
			ignore(empty_clip.(idx) <- p); fill_in_clip_instr_assigns empty_clip tail env lineno
	|	Pattern(_)::tail ->raise (Invalid_argument ("clip arguments may not mix styles",lineno))
	| 	_	-> raise (Invalid_argument ("clip arguments must all evaluate to instrument assignments",lineno))



let make_clip argVals env lineno =
	try
	(
		let instrument_list = get_key_from_env env "instruments" in
		let num_instrs =
		(
			match instrument_list with
				Instruments(i) -> List.length i
			|	_              -> raise (Failure "can't happen")
		) in
		let new_clip = emptyClip num_instrs in
		let first_arg = List.hd argVals in
		(
			match first_arg with
				Pattern(_) -> fill_in_clip_patterns new_clip argVals 0 lineno
			|	InstrumentAssignment(_,_) ->fill_in_clip_instr_assigns new_clip argVals env lineno
			|	_ -> raise (Invalid_argument ("clip arguments must be patterns or instrument assignments", -1))
		)
	)
	with Undefined_identifier("instruments",i) -> raise (Illegal_assignment ("trying to create a clip before defining instruments", i))

