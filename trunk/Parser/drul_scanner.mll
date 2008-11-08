{
        let num_keywords = ref 0 (*pointer*)
        let id           = ref 0 (*pointer*)
        let ints         = ref 0 (*pointer*)
        let num_comments = ref 0 (*pointer*)
        let num_tokens   = ref 0 (*pointer*)
        let s_id         = ref 0 (*pointer*)
        let string_const = ref 0 (*pointer*)
        
}

let digit         =  ['0' - '9']+
let identifier    =  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*


rule token = parse

        [' ' '\t' '\r' '\n']    { token lexbuf                                  }
   |    '('                             { LPAREN                                            } 
   |    ')'                             { RPAREN                                            } 
   |    '{'                             { LBRACE                                            } 
   |    '}'                             { RBRACE                                            }
   |    ';'                             { SEMI                                              } 
   |    ','                             { COMMA                                             }
   |    '+'                             { PLUS                                              } 
   |    '-'                             { MINUS                                             }
   |    '*'                             { TIMES                                             } 
   |    '/'                             { DIVIDE                                            }
   |    '='                             { ASSIGN                                            } 
   |    "=="                            { EQ                                                }
   |    "!="                            { NEQ                                               } 
   |    '!'                             { NOT                                               }
   |    '%'                             { MOD                                               }
   |    '<'                             { LT                                                }
   |    "<="                            { LEQ                                               } 
   |    '>'                             { GT                                                }
   |    ">="                            { GEQ                                               }
   |    "&&"                            { AND                                               }
   |    "||"                            { OR                                                }
   |    '.'                             { MCALL                                             }
   |    "//"                            { incr num_comments ; comment lexbuf                }
   |    "elseif"                        { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "if"                            { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "else"                          { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "rand"                          { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "pattern"                       { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "true"                          { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "false"                         { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "concat"                        { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "slice"                         { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "clip"                          { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "instrument"                    { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "length"                        { incr num_tokens ; incr num_keywords ; token lexbuf}
   |    "mapper"                        { incr num_tokens ; incr num_keywords ; token lexbuf}

   |    "map"                           { incr num_tokens ; incr num_keywords ; token lexbuf}

   |    "print"                         { incr num_tokens ; incr num_keywords ; token lexbuf}

   |    "output"                        { incr num_tokens ; incr num_keywords ; token lexbuf}
 
   |    "return"                        { incr num_tokens ; incr num_keywords ; token lexbuf}

   |    '$'digit                        { incr num_tokens ; incr s_id ; token lexbuf        }
   |    identifier      as ide          { if((String.length ide) <= 64)then ID(ide)                                       
                                          else
                                             (
                                               raise (Failure("ID TOO LONG: " ^
                                                          Char.escaped ide))
                                             )                                              }

 
   |    digit           as dig          { LITERAL(int_of_string dig)                        }

   |    '"'                             { const lexbuf                                      }

   |    eof                             { EOF                                               }

   |    _               as char         { raise (Failure("illegal character " ^
                                                          Char.escaped char))               }

and comment = parse

        '\n'                            { token lexbuf                                      }
      
   |    _                               { comment lexbuf                                    }


and const  = parse
        '"'                             { token lexbuf                                      }

   |    _                               { const lexbuf  (* NEED TO CAPTURE THE STRING*)     }


{
   let main () =
     let lexbuf = Lexing.from_channel stdin in
     token lexbuf;
     Printf.printf "\n There were %d keywords and %d identifiers in the input" !num_keywords !id;
     Printf.printf "\n There were %d integers and %d comments" !ints !num_comments;
     Printf.printf "\n There were %d special variables in all." !s_id;
     Printf.printf "\n There were %d s_consts in all." !string_const;
     Printf.printf "\n There were %d tokens in all.\n" !num_tokens
   let _ = Printexc.print main ()
}