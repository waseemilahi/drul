%{ open Ast %}

%start program
%type <Ast.program> program

%%

program:

       program {(*dummy file: nothing in it so far*)}

;
