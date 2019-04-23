(* This file was copied from Tommaso's setvariants *)

%{
	open Syntax.Eager
	module CD = Cduce_lib
	open Types
	open Primitives
%}

/* Token declarations. */

%token UNIT
%token DOT COLON COMMA BACKTICK
%token PAROPEN PARCLOSE
%token MOD FUN ARROW REC TIMES PRED SUCC FST SND


%token <string> IDENT
%token <string> PAT 
%token EOL ENDEXPR
%token LET EQ IN AND
%token IF THEN ELSE PLUS MINUS 

%left IN
%left PLUS MINUS EQ
%left IDENT
%nonassoc PARCLOSE ELSE FUN
%left TIMES 
%nonassoc MOD
%nonassoc PAT PAROPEN
%left SUCC PRED



/* Starting production. */	



%start prog
%type <Syntax.Eager.prog>			   prog 
%type <Syntax.Eager.e>              expr
%%

/* Parser definition. */

prog:
	| EOL 
		{ Eol }
	| e=expr ENDEXPR 
		{ Expr e }

expr:
	| e=app_expr
			{ e }
	| IF c=expr THEN e1=expr ELSE e2=expr
			{ Ifz (c, e1, e2) }
	| PAROPEN e1=expr PARCLOSE e2=expr
			{ App (e1, e2) }
	| p=primop
			{ p }	
	| f=fun_expr
			{ f }
	| p=pair 
			{ p }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| b=binop  
			{ b }
	| l=let_pattern
			{ l }
	| UNIT 
			{ Unit }

primop:	
	| PRED e=expr 
			{ Pred (e) }
	| SUCC e=expr
			{ Succ (e) }
	| FST e=expr
			{ Fst e }
	| SND e=expr
			{ Snd e }

app_expr:
	| e=a_expr 
		{ e }
	| e1=app_expr e2=a_expr
		{ App (e1, e2) }

pair:
	| PAROPEN e1=expr COMMA e2=expr PARCLOSE
		{ Pair (e1, e2) }

a_expr:
	| PAROPEN e=expr PARCLOSE 
		{ e }
	| v=var
			{ Var v }
	| c=pat_const
			{ Cst c }

fun_expr:
	| FUN t=pat x=var fun_delim e=expr   
			{ Lam (t, x, e) }
	| FUN xs=id_list fun_delim e=expr
			{ let rec currify e = function
				| [] -> e
				| x :: xs -> Lam (qfun (), x, currify e xs)
			  in currify e xs }

id_list:
	| x=var
		{ [x] }
	| x=var xs=id_unit_list
		{ x :: xs }

var_or_unit:
	| UNIT
		{ mk_var "_" }
	| x=var 
		{ x }

id_unit_list:
	| vu=var_or_unit
		{ [vu] }
	| vu=var_or_unit ids=id_unit_list
		{ vu :: ids }

binop:
	| e1=expr TIMES e2=expr 
		{ Mult (e1, e2) }
	| e1=expr PLUS e2=expr
		{ Plus (e1, e2) }
	| e1=expr MINUS e2=expr
		{ Minus (e1, e2) }
	| e1=expr EQ e2=expr
		{ Eq (e1, e2) }
	

let_pattern:
	| LET x=var EQ e1=expr IN e2=expr
			{ Let (x, e1, e2) }
	| LET f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lam (t, x, e1), e2) }
	| LET f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lam (cap t1 t2, x, e1), e2) }
	| LET f=var x=var EQ e1=expr IN e2=expr
			{ Let (f, Lam (qfun (), x, e1), e2) } 
	| LET REC f=var EQ e1=expr IN e2=expr
			{ Letrec (f, e1, e2) }
	| LET REC f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Letrec (f, Lam (t, x, e1), e2) }
	| LET REC f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Letrec (f, Lam (cap t1 t2, x, e1), e2) }
	| LET REC f=var x=var EQ e1=expr IN e2=expr
			{ Letrec (f, Lam (qfun (), x, e1), e2) } 
	| LET REC idf=var EQ f=fun_expr IN e2=expr
			{ match f with
			  | Lam (t, x, e) -> Letrec (idf, Lam (t, x, e), e2)
			  | _ -> failwith "rec used without a function" }
(*	| m=and_let
			{ a }
			
and_let: 
	| LET REC f=var COLON t=pat EQ FUN x=var fun_delim e1=expr AND e3=let_pattern IN e2=expr 
			{ match e3 with
			  | Letrec (g, e1', e2') -> 
			  		Letrec (f, Lam (t, x, Letrec(g, e1',), e2) } *)


fun_delim : DOT {} | ARROW {}



var:
	| v=IDENT 
			{ mk_var v }

pat_const:
	| BACKTICK id=IDENT
			{ parse_cst ("`" ^ id) } 
	| c=PAT
	 		{ parse_cst c }

pat:
	| t1=pat ARROW t2=pat
		{ mk_arrow t1 t2 }
	| BACKTICK id=IDENT
		{ parse_t ("`" ^ id) } 
	| t=PAT    			    
		{ parse_t t }   
