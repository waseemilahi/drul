%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ EOF MCALL AND OR NOT MOD
%token <int> LITERAL
%token <string> ID

%start program
%type<Ast.program> program

%%

program:

       EOF {(*dummy file: nothing in it so far*)}

;
