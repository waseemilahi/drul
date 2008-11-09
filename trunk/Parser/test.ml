open Drul_ast

let _ = 
let lexbuf = Lexing.from_channel stdin in 
let program = Drul_parser.program Drul_scanner.token lexbuf in 
print_endline "Parsed program (somewhat) successfully!"
(*let listing = Printer.string_of_program program in 
print_string listing *)
