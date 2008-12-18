(* helper functions for fancy forms of output 
	* created by Ben Warfield
	* 12/17/2008
*)


open Drul_types
open Drul_helpers

(* Oh, Printf.sprintf... we've only just met, and yet already I hate you with
 * a grim, joyless spite that would do a COBOL programmer proud.
 *)

let lilypond_staff_format  = (
	"\t\\new DrumStaff\n\t\\with{ \n\t\tinstrumentName = \"%s\"\n\t\tdrumStyleTable = #percussion-style \n\t\t\\override StaffSymbol #'line-count = #1\n\t\t\\remove Time_signature_engraver	\n\t}\n\t\\drummode { %s }\n"
	: ('a -> 'b -> 'c, unit, string) format 
	)

let lilypond_page_format = (
	"\\header{\n\ttitle = \"%s\"\n}\n<<\n%s\n>>\n\\version \"2.10.33\"\n"
	: ('a -> 'b -> 'c, unit, string) format 
)

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

