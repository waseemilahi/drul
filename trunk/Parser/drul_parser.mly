%{ open Drul_ast 
let debug str =  if (true) then ignore(print_endline str) else ignore()
%}

%token IF ELSE ELSEIF RETURN
%token TRUE FALSE
%token MAP MAPDEF LARROW CLIP
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ EOF MCALL AND OR NOT MOD
%token <int> LITERAL
%token <string> ID
%token <string> STRCONST

%left LIST /* is this correct? I mean, it *works*, but... */
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
    |   expr MCALL ID LPAREN RPAREN { MemberCall($1,$3,[]) }
    |   expr MCALL ID LPAREN expr_list RPAREN  { MemberCall($1,$3,$5) }
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
    |   ID LPAREN expr_list RPAREN { FunCall($1, $3) }     
    |   ID LPAREN RPAREN { FunCall($1,[]) }
    |   LPAREN expr RPAREN { $2} 
  /* this has a shift-reduce conflict with function calls */
    |   MAP LPAREN expr_list RPAREN block
        { MapCall(AnonyMap($5), $3)}
    |   MAP LPAREN expr_list RPAREN ID
        { MapCall(NamedMap($5), $3)}
    |   CLIP LPAREN clip_list RPAREN { MakeClip($3) }

statement:
        expr SEMI { Expr($1) }
    |   RETURN expr SEMI { Return($2) }
    |   MAPDEF ID LPAREN id_list RPAREN block
            { MapDef($2, List.rev $4, $6) }
    |   ID ASSIGN expr SEMI{ Assign($1,$3) }
    |   IF LPAREN expr RPAREN block iftail
        { IfBlock($3, $5, $6) }
        /* TODO : ELSEIF */
        /* May require AST change */
    | 	SEMI { EmptyStat }

clip_elem :
        ID	{ InstrAssign("",$1) }
    |   ID LARROW ID	{ InstrAssign ($1,$3) }


clip_list:
    clip_elem { [$1] }
    | clip_elem COMMA clip_list { $1::$3 }

block:
    LBRACE st_list RBRACE { List.rev $2 }

id_list:
    ID { [$1] }
    | id_list COMMA ID { $3::$1 }

expr_list:
     expr { [$1] }
    | expr COMMA expr_list { $1::$3 }

st_list:
    /* staring into the abyss */ { [] }
    | st_list statement { $2::$1 } /* build statement list backward */
    
program:
    st_list { Content(List.rev $1) }

iftail:
        ELSEIF LPAREN expr RPAREN block iftail
            { Some([ IfBlock($3,$5,$6)  ]) }
    |   ELSE block { Some($2) }
    |  /* nothing */ { None }
;
