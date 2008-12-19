(* helper functions for all non-trivial forms of output 
	* created by Ben Warfield 
	* (contents also authored partially by Rob Stewart--this file is a refactor)
	* 12/17/2008
*)


open Drul_types
open Drul_helpers

(* Oh, Printf.sprintf... we've only just met, and yet already I hate you with
 * a grim, joyless spite that would do a COBOL programmer proud.
 *)

let lilypond_staff_format  = (
	"	\\new DrumStaff\n\t\\with{
		instrumentName = \"%s\"
		drumStyleTable = #percussion-style 
		\\override StaffSymbol #'line-count = #1
		\\remove Time_signature_engraver	\n\t}\n\t\\drummode { %s }\n"
	: ('a -> 'b -> 'c, unit, string) format 
	)

let lilypond_page_format = (
	"\\header{\n\ttitle = \"%s\"\n}\n<<\n%s\n>>\n\\version \"2.10.33\"\n"
	: ('a -> 'b -> 'c, unit, string) format 
)

let string_of_beat b =
	let state = state_of_beat b in
	match state with
		None    -> "NULL"
	|	Some(b) -> if b then "NOTE" else "REST"


(* turn a pattern into a string, using predefined strings for "yes" and "no" *)
let folded_pattern p ifyes ifno =
	List.fold_left (fun a x -> a ^ (if x then ifyes else ifno)) "" p


(* get a string out of a pattern, pattern("0101") becomes "0101" *)
let string_of_pattern p = folded_pattern p "1" "0"

(* get a midge-formatted string for the supplied instrument out of a pattern *)
let string_of_instr_pattern p i = folded_pattern p (i ^ " ") "r "


(* problem: getting the name in makes this less generic *)
let lilypond_staff_of_pattern iname p = 
	let note_string = folded_pattern p "tri4 " "r4 " in
	let tmp = lilypond_staff_format  in
	Printf.sprintf tmp iname note_string 




let lilypond_page_of_clip clip_contents env title =
	let inames = get_instr_name_array env in
	assert ((Array.length inames) >= (Array.length clip_contents));
	let staff_strings = Array.mapi 
		(fun idx pat -> lilypond_staff_of_pattern inames.(idx) pat) 
		clip_contents in
	let all_staffs = Array.fold_left (fun a b -> a ^ b) "" staff_strings in
	Printf.sprintf lilypond_page_format title all_staffs

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
			then (
				"\t@channel 10 " ^ inames.(idx) ^ " { /L4/" 
				^ (string_of_instr_pattern p inames.(idx)) ^ " }\n"
			)
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