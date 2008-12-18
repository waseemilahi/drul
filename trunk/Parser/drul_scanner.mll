{
	open Drul_parser
	open Lexing
	let debugging   = ref false
	let standalone  = ref false
	let line_number = ref 1
	let set_debug() = debugging := true
	let debug str   = if (!debugging) then ignore(print_endline str) else ignore()
	let escape_re   = Str.regexp "\\\\\\(\\\\\\|\"\\)"
	(* "\\\\\\([\\\"]\\)" also works, almost as ugly *)
	let escape_repl = "\\1"


	(* In 3.11 this is built in to Lexing, but alas, I have 3.10...*)
	(* This code largely borrowed from a newgroup post by Till Varoquaux 
	 * complaining about it not being built in: 
	 * http://caml.inria.fr/pub/ml-archives/caml-list/2008/03/4575c51493931878a25de6b1712a4b24.en.html
	 *)
	let new_line lexbuf = 
	incr line_number; 
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { 
		pos with 
			pos_lnum = pos.pos_lnum + 1;
			pos_bol = pos.pos_cnum 
	}

}

let digit      =  ['0' - '9']+
let identifier =  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
		' '                             { debug("whitespace 'b '"); token lexbuf }
   |    '\t'                            { debug("whitespace 't'");  token lexbuf }
   |    '\r'                            { debug("whitespace 'r'");  token lexbuf }
   |    '\n'                            { debug("whitespace 'n'"); 
                                            new_line lexbuf;
                                            token lexbuf }
   |    "//"                            { debug "COMMENT"; comment lexbuf }
   |    '('                             { debug "LPAREN"; LPAREN(!line_number) }
   |    ')'                             { debug "RPAREN"; RPAREN(!line_number) }
   |    '{'                             { debug "LBRACE"; LBRACE(!line_number) }
   |    '}'                             { debug "RBRACE"; RBRACE(!line_number) }
   |    ';'                             { debug "SEMI";   SEMI(!line_number)   }
   |    ','                             { debug "COMMA";  COMMA(!line_number)  }
   |    '+'                             { debug "PLUS";   PLUS(!line_number)   }
   |    '-'                             { debug "MINUS";  MINUS(!line_number)  }
   |    '*'                             { debug "TIMES";  TIMES(!line_number)  }
   |    '/'                             { debug "DIVIDE"; DIVIDE(!line_number) }
   |    '='                             { debug "ASSIGN"; ASSIGN(!line_number) }
   |    "=="                            { debug "EQ";  EQ(!line_number)  }
   |    "!="                            { debug "NEQ"; NEQ(!line_number) }
   |    '!'                             { debug "NOT"; NOT(!line_number) }
   |    '%'                             { debug "MOD"; MOD(!line_number) }
   |    '<'                             { debug "LT";  LT(!line_number)  }
   |    "<="                            { debug "LEQ"; LEQ(!line_number) }
   |    '>'                             { debug "GT";  GT(!line_number)  }
   |    ">="                            { debug "GEQ"; GEQ(!line_number) }
   |    "&&"                            { debug "AND"; AND(!line_number) }
   |    "||"                            { debug "OR";  OR(!line_number)  }
   |    '.'                             { debug "MCALL"; MCALL(!line_number)   }
   |    "true"                          { debug "TRUE";  TRUE(!line_number)    }
   |    "false"                         { debug "FALSE"; FALSE(!line_number)   }
   |    "if"                            { debug "IF";     IF(!line_number)     }
   |    "else"                          { debug "ELSE";   ELSE(!line_number)   }
   |    "elseif"                        { debug "ELSEIF"; ELSEIF(!line_number) }
   |    "mapper"                        { debug "MAPDEF"; MAPDEF(!line_number) }
   |    "map"                           { debug "MAP";    MAP(!line_number)    }
   |    "return"                        { debug "RETURN"; RETURN(!line_number) }
   |    "instruments"                   { debug "INSTRUMENTS"; INSTRUMENTS(!line_number) }
   |    "<-"                            { debug "LARROW"; LARROW(!line_number) }
   |    '$' digit as numbers            { debug("index variable " ^ numbers); ID(numbers, !line_number) }
   |    identifier as ide               {
											if ((String.length ide) <= 64)
											then
											(
												debug("identifier " ^ ide);
												ID(ide,!line_number)
											)
											else raise (Failure("ID TOO LONG: " ^ ide))
										}
   |    digit as dig                    { debug ("digits " ^ dig); INTLITERAL(int_of_string dig, !line_number) }
   |    '"' (( ('\\' [ '"' '\\' ] ) | [^ '\r' '\n' '\\' '"'] )* as rawstr) '"'
										{
											(* TODO: accept newlines, then raise "illegal character in string?" *)
											let fixedstr = Str.global_replace escape_re escape_repl rawstr in
											debug(("string constant [" ^ fixedstr ^ "]"));
											STRLITERAL(fixedstr, !line_number)
										}
   |    eof                             { debug "EOF"; EOF(!line_number) }
   |    _  as char                      { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
		'\n'                            { new_line lexbuf; token lexbuf }
   |    eof                             { debug "EOF"; EOF(!line_number) }
   |    _                               { comment lexbuf                 }


{
	if (!standalone) then
	 let lexbuf = Lexing.from_channel stdin in
	 let rec nexttoken buf = ignore(token buf);nexttoken buf
	 in nexttoken lexbuf
	else ignore()
}
