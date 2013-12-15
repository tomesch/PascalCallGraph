%{
open Printf;;
open Syntax;;
%}

%token <int> NUM
%token <string> ID
%token <bool> BOOL
%token BEGIN END DOT
%token PROGRAM
%token FUNCTION PROCEDURE VAR
%token INTEGER BOOLEAN ARRAY OF TYPE NEW
%token LT LE GT GE EQ NE OR AND NOT
%token LPAREN RPAREN LBRACKET RBRACKET
%token COLONEQ COLON SEMICOLON COMMA
%token IF THEN ELSE WHILE DO 
%token PLUS MINUS TIMES DIV 
%token WRITE WRITELN READ READLN
%token COMMENT

%left LT LE GT GE EQ NE
%left AND OR
%left PLUS MINUS 
%left TIMES DIV 
%nonassoc UMINUS
%nonassoc LBRACKET 
%nonassoc NOT

%start input
%type <Syntax.program> input

%%
input: 
	| PROGRAM vars definitions instructions DOT { $2 , $3 , $4 }
;
vars: { [] }
	| VAR var SEMICOLON vars1 { $2 :: $4 }
;
vars1: { [] }
	| var SEMICOLON vars1 { $1 :: $3 }
;
var: 
	| idlist COLON type_expr { $1 , $3 }
;
idlist: 
	| ID 				{ [$1] }
	| ID COMMA idlist 	{ $1 :: $3 }
;
definitions:	
	| 									{ [] }
	| fonctions SEMICOLON definitions 	{ $1 :: $3 }
;
fonctions: 
	| FUNCTION ID LPAREN args RPAREN COLON type_expr SEMICOLON vars instructions 	{ $2, ($4, Some $7, $9, $10 ) }
	| PROCEDURE ID LPAREN args RPAREN SEMICOLON vars instructions 					{ $2, ($4, None , $7, $8 ) } 	
;
args: 
	| 											{ [] }
	| ID args1 COLON type_expr 					{ [($1 :: $2 , $4)] } 
	| ID args1 COLON type_expr SEMICOLON args 	{ ($1 :: $2 , $4) :: $6 }
;
args1: 
	| 					{ [] }
	| COMMA ID args1 	{ $2 :: $3 }
;
instructions:
	| ID COLONEQ expr 									{ Set ($1, $3) }
	| BEGIN blocs END 									{ Sequence $2 }
	| IF expr THEN instructions ELSE instructions 		{ If ($2, $4, $6) }
	| WHILE expr DO instructions 						{ While ($2, $4) }
	| ID LPAREN arguments RPAREN 						{ Procedure_call ($1, $3) }
	| WRITE LPAREN expr RPAREN      					{ Write ($3) }
	| WRITELN LPAREN expr RPAREN    					{ Writeln ($3) }
	| expr LBRACKET expr RBRACKET COLONEQ expr 			{ Seti ($1, $3, $6) }
;
blocs: 
	| 					{ [] }
	| instructions bloc { $1 :: $2 }
;
bloc: 
	| 								{ [] }
	| SEMICOLON instructions bloc 	{ $2 :: $3 }
;
arguments: 
	| 				{ [] }
	| expr argument { $1 :: $2 }
;
argument: 
	| 						{ [] }
	| COMMA expr argument 	{ $2 :: $3 }
;
expr: 
	| LPAREN expr RPAREN 			{ $2 }
	| READ LPAREN RPAREN         	{ Read }
	| READLN LPAREN RPAREN       	{ Readln }
	| NUM 							{ Int $1  }
	| MINUS expr 					{ Un (UMinus, $2) }
	| BOOL 							{Bool $1 }
	| NEW type_expr 				{ New $2 }
	| ID 							{ Get $1 }
	| ID LPAREN arguments RPAREN 	{ Function_call ($1, $3) }
	| expr LBRACKET expr RBRACKET 	{ Geti ($1, $3) }
	| expr PLUS expr 				{ Bin (Plus, $1, $3) }
	| expr MINUS expr 				{ Bin (Minus, $1, $3)}
	| expr TIMES expr 				{ Bin (Times, $1, $3) }
	| expr DIV expr 				{ Bin (Div, $1, $3) }
	| expr LT expr 					{ Bin (Lt, $1, $3) }
	| expr LE expr 					{ Bin (Le, $1, $3) }
	| expr GT expr 					{ Bin (Gt, $1, $3) }
	| expr GE expr 					{ Bin (Ge, $1, $3) }
	| expr EQ expr 					{ Bin (Eq, $1, $3) }
	| expr NE expr 					{ Bin (Ne, $1, $3) }
	| expr OR expr 					{ Bin (Or, $1, $3) }
	| expr AND expr 				{ Bin (And, $1, $3) }
	| NOT expr 						{ Un (Not, $2) }
;
type_expr : 
	| INTEGER 				{ Integer }
	| BOOLEAN 				{ Boolean }
	| ARRAY OF type_expr 	{ Array ($3) }
;
%%