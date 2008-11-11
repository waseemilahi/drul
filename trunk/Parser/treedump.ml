open Drul_printer

let _ = 
let lexbuf = Lexing.from_channel stdin in 
let program = Drul_parser.program Drul_scanner.token lexbuf in 
print_endline (string_of_program program)