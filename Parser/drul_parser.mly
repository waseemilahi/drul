%{ open Ast %}

%start program
%type <Ast.program> program

%%

program:

       program {}

;
