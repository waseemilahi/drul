%{ open Drul_ast 
let debug str =  if (true) then ignore(print_endline str) else ignore()
%}

%token IF ELSE ELSEIF RETURN
%token TRUE FALSE
%token MAP MAPDEF
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ EOF MCALL AND OR NOT MOD
%token <int> LITERAL
%token <string> ID
%token <string> STRCONST

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
        LITERAL     { CInt($1) }
    |   STRCONST    { CStr($1) }
    |   TRUE        { CBool(true) }
    |   FALSE       { CBool(false)}
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
        expr SEMI { Expr($1) }
    |   MAPDEF ID LPAREN id_list RPAREN LBRACE st_list RBRACE
            { MapDef($2, List.rev $4, List.rev $7) }
    |   ID ASSIGN expr { Assign($1,$3) }
    |   IF LPAREN expr RPAREN LBRACE st_list RBRACE
        { IfBlock($3,List.rev $6, None) } /* no else yet */
id_list:
    /* invalid empty argument list */ { [] }
    | id_list ID { $2::$1 }

st_list:
    /* staring into the abyss */ { [] }
    | st_list statement { $2::$1 } /* build statement list backward */
    
program:
    st_list { Content(List.rev $1) }

      

;
