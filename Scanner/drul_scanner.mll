{
    (* We will associate the input with tokens gotton from the parser
	and also add restrictions to the ones we have *)

	let num_keywords = ref 0 (*pointer*)
	let id		 = ref 0 (*pointer*)
	let ints	 = ref 0 (*pointer*)

}

let digit = ['0' - '9']
let identifier = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*


rule token = parse

	[' ' '\t' '\r'] 	        { token lexbuf }

   |	"//"				{comment lexbuf}

   | 	'\n'				{ print_endline "FINISH!"}

   |	"NULL"				{incr num_keywords ; token lexbuf }

   |	"elseif"			{incr num_keywords ; token lexbuf }

   | 	"if"				{incr num_keywords ; token lexbuf }

   |	"else"				{incr num_keywords ; token lexbuf }

   |	"rand"				{incr num_keywords ; token lexbuf }

   |	"pattern"			{incr num_keywords ; token lexbuf }

   |	"concat"			{incr num_keywords ; token lexbuf }

   | 	"slice"				{incr num_keywords ; token lexbuf }

   |	"clip"				{incr num_keywords ; token lexbuf }

   |	"instrument"			{incr num_keywords ; token lexbuf }

   | 	"length"			{incr num_keywords ; token lexbuf }

   |	"maper"				{incr num_keywords ; token lexbuf }

   |	"map"				{incr num_keywords ; token lexbuf }

   |	"print"				{incr num_keywords ; token lexbuf }

   |	"output"			{incr num_keywords ; token lexbuf }

   | 	"return"			{incr num_keywords ; token lexbuf }

   |    identifier			{incr id ; token lexbuf }

   |	digit+				{incr ints ; token lexbuf}

   | 	_				{ token lexbuf}


and comment = parse

       	'\n'				{token lexbuf}
      
   |	_				{comment lexbuf}

{
   let main () =
     let lexbuf = Lexing.from_channel stdin in
     token lexbuf;
     Printf.printf " We are done! There were %d keywords and %d identifiers in the input" !num_keywords !id;
     Printf.printf "\n There were %d integers" !ints
   let _ = Printexc.print main ()
  
}