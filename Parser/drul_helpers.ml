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
		|	_            -> raise (Failure "in add_pattern_alias, should not happen")
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
	| _ -> raise (Failure "in maxlen_helper, should not happen (not a pattern?)")

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
let rec get_key_from_env env key lineno =
	if NameMap.mem key env.symbols then NameMap.find key env.symbols
	else match env.parent with
			Some(parent_env) -> get_key_from_env parent_env key lineno
		|	None -> raise (Undefined_identifier (key, lineno))

(* takes an alias, turns it into a beat object (used in mapper) *)
and  beat_of_alias env alias lineno =
	let currentVar = get_key_from_env env "$current" lineno
	in match currentVar with
			Int(currentVal) -> Beat(alias,currentVal)
		|	_  -> raise (Failure "in beat_of_alias, can't have a non-integer in $current")

(* turn a pattern into a string, using predefined strings for "yes" and "no" *)
let folded_pattern p ifyes ifno =
	List.fold_left (fun a x -> a ^ (if x then ifyes else ifno)) "" p


(* get a string out of a pattern, pattern("0101") becomes "0101" *)
let string_of_pattern p = folded_pattern p "1" "0"

(* get a midge-formatted string for the supplied instrument out of a pattern *)
let string_of_instr_pattern p i = folded_pattern p (i ^ " ") "r "

let state_of_beat beat =
	match beat with
		Beat(pattern_data,idx) ->
			let pattern_length = Array.length pattern_data in
			if (idx < 0 or idx >= pattern_length) then None else Some(pattern_data.(idx))
	|	_ -> raise (Failure "in state_of_beat, should not happen (not a beat?)")

(* get an array with the names of the current instruments in it *)
let get_instr_name_array env =
	(* TODO: make this a less hackish way to avoid passing that line-number around? *)
	let drulInstrList = get_key_from_env env "instruments" 0 in
	match drulInstrList with 
			Instruments(l) -> Array.of_list l
		| _ -> raise (Failure "slot for instruments does not contain instruments")


(* 
find the position of an instrument in the instruments in the env, returns -1 if doesn't find it 
*)
let get_instrument_pos env instrName lineno =
	try
		let instrListDrul = get_key_from_env env "instruments" lineno in
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
		|	_ -> raise (Failure "in get_instrument_pos, weird stuff in env for instruments...")
	with
		Undefined_identifier(e,i) -> raise (Failure "in get_instument_pos, instrument not saved in env yet")
	|	Failure(e)                -> raise (Failure e)
	|	_                         -> raise (Failure "in get_instrument_pos, wrong or new exception")

(* concat patterns into one *)
let rec concat_pattern_list plist lineno =
	match plist with
		[]	-> []
	|	Pattern(x)::tail -> x @ (concat_pattern_list tail) lineno
	| 	_ -> raise (Invalid_argument ("concat only concatenates patterns", lineno))


(* 
get an empty clip (clip with the right number of empty patterns)
and fills it from a pattern list
*)
let rec fill_in_clip_patterns empty_clip pattern_list idx lineno = match pattern_list with
		[] -> Clip(empty_clip) (* not technically empty any more *)
			(* TODO: catch array out of bounds here *)
	|	Pattern(p)::tail -> ignore(empty_clip.(idx) <- p); fill_in_clip_patterns empty_clip tail (idx + 1) lineno
	|	InstrumentAssignment(_,_)::tail -> raise (Invalid_argument ("clip arguments may not mix styles", lineno))
	| 	_ -> raise (Invalid_argument ("clip arguments must all evaluate to patterns", lineno))

(*
similar as fill_in_clip_patterns, but deals with the InstrumentAssignments 'hihat' <- pattern("1")
*)
let rec fill_in_clip_instr_assigns empty_clip assignment_list env lineno = match assignment_list with
		[]	-> Clip(empty_clip) (* not technically empty any more *)
	|	InstrumentAssignment(instrName,p)::tail ->
			let idx = get_instrument_pos env instrName lineno in
			  if idx < 0 
			  then raise (Invalid_argument ("unknown instrument name '" ^ instrName ^"'",lineno))
			  else
			ignore(empty_clip.(idx) <- p); fill_in_clip_instr_assigns empty_clip tail env lineno
	|	Pattern(_)::tail ->raise (Invalid_argument ("clip arguments may not mix styles",lineno))
	| 	_	-> raise (Invalid_argument ("clip arguments must all evaluate to instrument assignments",lineno))


(* first function in order to make a clip *)
let make_clip argVals env lineno =
	try
	(
		let instrument_list = get_key_from_env env "instruments" lineno in
		let num_instrs =
		(
			match instrument_list with
				Instruments(i) -> List.length i
			|	_              -> raise (Failure "in make_clip, should not happen")
		) in
		let new_clip = emptyClip num_instrs in
		let first_arg = List.hd argVals in
		(
			match first_arg with
				Pattern(_) -> fill_in_clip_patterns new_clip argVals 0 lineno
			|	InstrumentAssignment(_,_) ->fill_in_clip_instr_assigns new_clip argVals env lineno
			|	_ -> raise (Invalid_argument ("clip arguments must be patterns or instrument assignments", lineno))
		)
	)
	with Undefined_identifier("instruments",i) -> raise (Illegal_assignment ("trying to create a clip before defining instruments", i))

let string_of_clip clip_contents env =
	let instrument_names = get_instr_name_array env in
	assert ((Array.length instrument_names) >= (Array.length clip_contents));
	let formatted_strings = Array.mapi 
		(fun idx p -> instrument_names.(idx) ^":\t" ^ string_of_pattern p) 
		clip_contents in
	let all_patterns = Array.fold_left 
		(fun a str -> a ^ "\t" ^ str ^ "\n") 
		"" formatted_strings in
	"[\n" ^ all_patterns ^ "]"
	


let midge_of_clip clip_contents env tempo =
	let inames = get_instr_name_array env in
	assert ((Array.length inames) >= (Array.length clip_contents));
	let pattern_strings = Array.mapi
		(fun idx p -> if (0 < List.length p) 
			then ("\t@channel 10 " ^ inames.(idx) ^ " { /L4/" ^ (string_of_instr_pattern p inames.(idx)) ^ " }\n")
			else ""
		)
		clip_contents in
	"@head {\n" 
		^ "$tempo " ^ (string_of_int tempo) ^ "\n" 
		^ "$time_sig 4/4" ^ "\n"
		^ "}\n"
		^"@body {\n"
		^ (Array.fold_left (fun a s->a^s) "" pattern_strings)
		^ "\n}\n"