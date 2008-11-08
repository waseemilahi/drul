%{ open Drul_ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ EOF MCALL AND OR NOT MOD
%token <int> LITERAL
%token <string> ID

%nonassoc NOELSE /* which we define somehow, somewhere */
%nonassoc ELSE
%left ASSIGN /* needs no associativity, may need no precedence? */
%left OR
%left AND
%left NEQ EQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc UMINUS NOT

%start program
%type<Drul_ast.program> program

%%
expr:
        LITERAL     { Lit($1) }
    |   ID          { Var($1) }
    |   expr PLUS expr { ArithBinop($1,Add,$3)  }
    |   expr MINUS expr { ArithBinop($1,Sub,$3) }
    |   expr TIMES expr { ArithBinop($1,Mult,$3) }
    |   expr DIVIDE expr { ArithBinop($1, Div, $3) }
    |   expr MOD    expr { ArithBinop($1, Mod, $3) }
    |   expr EQ expr { Comparison($1,EqualTo,$3) }
    |   expr NEQ expr { Comparison($1,NotEqual,$3) }
    |   expr LT expr { Comparison($1,LessThan,$3) }
    |   expr GT expr { Comparison($1,GreaterThan,$3) }
    |   expr LEQ expr { Comparison($1,LessEq,$3) }
    |   expr GEQ expr { Comparison($1,GreaterEq,$3) }
    |   expr AND expr { LogicBinop($1,And,$3) }
    |   expr OR expr { LogicBinop($1,Or,$3) }
    |   MINUS expr  %prec UMINUS { UnaryMinus($2) }
    |   NOT expr   { UnaryNot($2) }
    |   ID LPAREN expr RPAREN { FunCall($1, [$3]) }
    
statement:
        expr SEMI { Expr($1) } /* that'll do for now */
    
program:
       program statement { match $1 with Content(a)->Content($2 :: a) }

;
