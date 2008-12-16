%{ open Drul_ast
let debug str = if (true) then ignore(print_endline str) else ignore()
%}

%token <int> IF ELSE ELSEIF RETURN
%token <int> TRUE FALSE
%token <int> MAP MAPDEF LARROW CLIP
%token <int> SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token <int> ASSIGN EQ NEQ LT LEQ GT GEQ EOF MCALL AND OR NOT MOD
%token <int> INSTRUMENTS
%token <int> OUTPUT
%token <int * int>    INTLITERAL
%token <string * int> STRLITERAL ID

%left LIST /* is this correct? I mean, it *works*, but... */
%nonassoc NOELSE /* which we define somehow, somewhere */
%nonassoc ELSE
%left ASSIGN LARROW /* needs no associativity, may need no precedence? */
%left INSTRUMENTS /* same comment, may not need to be here */
%left OR
%left AND
%left NEQ EQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc UMINUS NOT
%left MCALL

%start program
%type<Drul_ast.program> program
%%

expr:
		INTLITERAL { {real_expr = CInt(fst($1)); lineno = snd($1) } }
	|   STRLITERAL { {real_expr = CStr(fst $1); lineno = snd($1)  } }
	|   TRUE       { {real_expr = CBool(true); lineno = $1  }       }
	|   FALSE      { {real_expr = CBool(false); lineno = $1  }      }
	|   ID         { {real_expr = Var(fst $1); lineno = snd($1)  }  }
	|   expr MCALL ID LPAREN RPAREN { 
	        {real_expr = MemberCall($1, fst($3), []) ; lineno = $2 }
	    }
	|   expr MCALL ID LPAREN expr_list RPAREN  { 
	        {real_expr = MemberCall($1, fst($3), $5) ; lineno = $2 }
	    }
	|	OUTPUT MCALL ID LPAREN expr_list RPAREN  { 
	       {real_expr = Output(fst($3), $5); lineno = $1} 
	    }
	|   expr PLUS   expr { 
	        {real_expr = ArithBinop($1, Add,  $3) ; lineno = $2 }
	    }
	|   expr MINUS  expr { 
	        {real_expr = ArithBinop($1, Sub,  $3) ; lineno = $2 }
	    }
	|   expr TIMES  expr { 
	        {real_expr = ArithBinop($1, Mult,  $3) ; lineno = $2 }
	    }
	|   expr DIVIDE expr { 
	        {real_expr = ArithBinop($1, Div,  $3) ; lineno = $2 }
	    }
	|   expr MOD    expr { 
	        {real_expr = ArithBinop($1, Mod,  $3) ; lineno = $2 }
	    }
	|   expr EQ  expr { 
	        { real_expr = Comparison($1, EqualTo, $3); lineno = $2 }
	    }
	|   expr NEQ expr { 
	        { real_expr = Comparison($1, NotEqual, $3); lineno = $2 }
	    }
	|   expr LT  expr { 
	        { real_expr = Comparison($1, LessThan, $3); lineno = $2 }
	    }
	|   expr GT  expr { 
	        { real_expr = Comparison($1, GreaterThan, $3); lineno = $2 }
	    }
	|   expr LEQ expr { 
	        { real_expr = Comparison($1, LessEq, $3); lineno = $2 }
	    }
	|   expr GEQ expr { 
	        { real_expr = Comparison($1, GreaterEq, $3); lineno = $2 }
	    }
	|   expr AND expr { 
	        { real_expr = LogicBinop($1, And, $3); lineno = $2 }
	    }
	|   expr OR  expr { 
	        { real_expr = LogicBinop($1, Or, $3); lineno = $2 }
	    }
	|   MINUS expr %prec UMINUS { 
	        { real_expr = UnaryMinus($2); lineno = $1 }
	    }
	|   NOT expr {
	        { real_expr = UnaryNot($2); lineno = $1 }
	    }
	|   ID LPAREN expr_list RPAREN { 
	        { real_expr = FunCall(fst($1), $3); lineno = snd($1) }
	    }
	|   ID LPAREN RPAREN { 
            { real_expr = FunCall(fst($1), []); lineno = snd($1) }
        }
	|   LPAREN expr RPAREN { {real_expr = $2.real_expr; lineno = $1} }
	|   MAP LPAREN expr_list RPAREN block { 
	        {real_expr = MapCall(AnonyMap($5), $3); lineno = $1 }
	    }
	|   MAP LPAREN expr_list RPAREN ID    { 
	        {real_expr = MapCall(NamedMap(fst($5)), $3); lineno = $1 }
	    }
	|   STRLITERAL LARROW expr {
	        {real_expr = InstrAssign(fst($1), $3); lineno = $2 }
	    }

statement:
		expr SEMI { Expr($1) }
	|   RETURN expr SEMI { Return($2) }
	|   MAPDEF ID LPAREN id_list RPAREN block { MapDef((fst $2), List.rev $4, $6, snd($2)) }
	|   ID ASSIGN expr SEMI { Assign(fst($1), $3, snd($1)) }
	|   IF LPAREN expr RPAREN block iftail { IfBlock($3, $5, $6) }
	|   INSTRUMENTS LPAREN expr_list RPAREN SEMI { InstrDef($3, $1) }
	|   INSTRUMENTS LPAREN RPAREN SEMI           { InstrDef([], $1) }
	| 	SEMI { EmptyStat }

block:
	LBRACE st_list RBRACE { List.rev $2 }

id_list:
		ID { [fst($1)] }
	|	id_list COMMA ID { fst($3)::$1 }

expr_list:
		expr { [$1] }
	|	expr COMMA expr_list { $1::$3 }

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
