{
    (* We will associate the input with tokens gotton from the parser. For now I am
	just counting and printing out number of tokens and other stuff. *)

	let num_keywords = ref 0 (*pointer*)
	let id		 = ref 0 (*pointer*)
	let ints	 = ref 0 (*pointer*)
        let num_comments = ref 0 (*pointer*)
	let num_tokens   = ref 0 (*pointer*)
	let s_id         = ref 0 (*pointer*)
	let miscs        = ref 0 (*pointer*)
	let string_const = ref 0 (*pointer*)

}

let digit         =  ['0' - '9']+
let identifier    =  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let misc_tokens   =  [ ';' ',' '.' '(' ')' '{' '}' '=']


rule token = parse

	[' ' '\t' '\r' '\n'] 	        { token lexbuf 					    }

   |	"//"				{incr num_comments ; comment lexbuf		    }

   |	"elseif"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   | 	"if"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"else"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"rand"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"pattern"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"true" 				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"false" 			{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"concat"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   | 	"slice"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"clip"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"instrument"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   | 	"length"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"mapper"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"map"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"print"				{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	"output"			{incr num_tokens ; incr num_keywords ; token lexbuf }
 
   | 	"return"			{incr num_tokens ; incr num_keywords ; token lexbuf }

   |	'$'['0' - '9']+			{incr num_tokens ; incr s_id ; token lexbuf  	    }

   |    identifier	as ide		{if((String.length ide) <= 64)
					   then (token lexbuf ; incr num_tokens ; incr id)   
					 else(print_endline ""  ;
					      print_string "THIS ID IS TOO LONG : ";
					      print_endline ide ; 
					      print_string " Only first error in code is";
					      print_endline " shown.";
					      exit(1) )  		                    }

   |	digit				{incr num_tokens ; incr ints ; token lexbuf	    }

   |	misc_tokens			{incr num_tokens ; incr miscs ; token lexbuf	    }

   |    '"'				{incr num_tokens ; incr string_const; const lexbuf  }
 
   | 	_	as wrong_token  	{ print_string "THIS IS A WRONG TOKEN: ";
					  print_char wrong_token ; print_endline "";
					  print_endline "Only first error is shown"; exit(1)}

   |    eof				{ print_endline "END OF FILE"			    }


and comment = parse

       	'\n'				{ token lexbuf}
      
   |	_				{comment lexbuf}

and const  = parse
	'"'				{token lexbuf}

   |   	_				{const lexbuf}

{
   let main () =
     let lexbuf = Lexing.from_channel stdin in
     token lexbuf;
     Printf.printf "\n There were %d keywords and %d identifiers in the input" !num_keywords !id;
     Printf.printf "\n There were %d integers and %d comments" !ints !num_comments;
     Printf.printf "\n There were %d special variables in all." !s_id;
     Printf.printf "\n There were %d miscs in all." !miscs;
     Printf.printf "\n There were %d s_consts in all." !string_const;
     Printf.printf "\n There were %d tokens in all.\n" !num_tokens
   let _ = Printexc.print main ()
  
}