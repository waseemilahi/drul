{
		open Drul_parser
		let debugging = ref false
		let standalone = ref false
		let num_keywords = ref 0 (* pointer *)
		let id           = ref 0 (* pointer *)
		let ints         = ref 0 (* pointer *)
		let num_comments = ref 0 (* pointer *)
		let num_tokens   = ref 0 (* pointer *)
		let s_id         = ref 0 (* pointer *)
		let string_const = ref 0 (* pointer *)
		let set_debug() = debugging := true
		let debug str =  if (!debugging) then ignore(print_endline str) else ignore()

}

let digit         =  ['0' - '9']+
let identifier    =  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*


rule token = parse
		[' ' '\t' '\r' '\n']            { debug( "whitespace");token lexbuf                }
   |    '('                             { debug "LPAREN"; LPAREN                         }
   |    ')'                             { debug "RPAREN"; RPAREN                         }
   |    '{'                             { debug "LBRACE"; LBRACE                         }
   |    '}'                             { debug "RBRACE"; RBRACE                         }
   |    ';'                             { debug "SEMI"; SEMI                             }
   |    ','                             { debug "COMMA"; COMMA                   	    }
   |    '+'                             { debug "PLUS"; PLUS                     	    }
   |    '-'                             { debug "MINUS"; MINUS                   	    }
   |    '*'                             { debug "TIMES"; TIMES                   	    }
   |    '/'                             { debug "DIVIDE"; DIVIDE                 		}
   |    '='                             { debug "ASSIGN"; ASSIGN                 	    }
   |    "=="                            { debug "EQ"; EQ                         	    }
   |    "!="                            { debug "NEQ"; NEQ                       	    }
   |    '!'                             { debug "NOT"; NOT                       	    }
   |    '%'                             { debug "MOD"; MOD                       	    }
   |    '<'                             { debug "LT"; LT                        		    }
   |    "<="                            { debug "LEQ"; LEQ                      		    }
   |    '>'                             { debug "GT"; GT                        		    }
   |    ">="                            { debug "GEQ"; GEQ                      		    }
   |    "&&"                            { debug "AND"; AND                      		    }
   |    "||"                            { debug "OR"; OR                                 }
   |    '.'                             { debug "MCALL"; MCALL                           }
   |    "//"                            { debug "COMMENT"; comment lexbuf                 }
   |    "true"                          { debug "TRUE"; TRUE  }
   |    "false"                         { debug "FALSE"; FALSE }
   |    "if"                            { debug "IF"; IF }
   |    "else"                          { debug "ELSE"; ELSE }
   |    "elseif"                        { debug "ELSEIF"; ELSEIF }
   |    "mapper"                        { debug "MAPDEF"; MAPDEF }
   |    "map"                           { debug "MAP"; MAP }
   |    "return"                        { debug "RETURN"; RETURN }
   |    '$'(digit as numbers)           { debug("index variable " ^ numbers); ID(numbers)   }
   |    identifier      as ide          {
											if ((String.length ide) <= 64) 
											then (debug("identifier " ^ ide); ID(ide))
											else
											(
												raise (Failure("ID TOO LONG: " ^ ide))
											)
										}

   |    digit           as dig          { debug ("digits " ^ dig); LITERAL(int_of_string dig)     }
   |    '"' (( ('\\' [ '"' '\\' ] ) | [^  '\\' '"'] )* as str) '"' { debug(("string constant " ^ str)); STRCONST(str) } 
   |    eof                             { debug "EOF"; EOF                               }
   |    _               as char         { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
		'\n'                            { token lexbuf                                      }
   |    eof                             { debug "EOF"; EOF                                  } 
   |    _                               { comment lexbuf                                    }


{
	if (!standalone) then
	 let lexbuf = Lexing.from_channel stdin in
	 let rec nexttoken buf = ignore(token buf);nexttoken buf
	 in nexttoken lexbuf
	else ignore()
}
