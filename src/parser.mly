(* This file was copied from Tommaso's setvariants *)

%{
	open Syntax.Eager
	module CD = Cduce_lib
	open Types
	open Primitives
%}

/* Token declarations. */

%token CAP CUP QMARK
%token STREAM
%token UNIT
%token DOT COLON COMMA BACKTICK
%token PAROPEN PARCLOSE
%token CAST FUN ARROW REC TIMES PRED SUCC FST SND
%token BRACKOPEN BRACKCLOSE


%token <string> IDENT
%token <string> PAT 
%token EOL ENDEXPR
%token LET EQ IN 
%token IF THEN ELSE PLUS MINUS MOD

%left IN
%left PLUS MINUS EQ
%left IDENT
%nonassoc PARCLOSE ELSE FUN
%left TIMES 
%nonassoc CAST
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
	| e=expr CAST t=pat
			{ Cast (e, (t, dom t)) }
	| b=binop  
			{ b }
	| l=let_pattern
			{ l }
	| UNIT 
			{ Cst (parse_cst "[]") }

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
	| FUN UNIT fun_delim e=expr 
			{ Mu (mk_arrow (parse_t "[]") any, fresh_var (), fresh_var (), e) }
	| FUN t=pat x=var fun_delim e=expr   
			{ Mu (t, fresh_var (), x, e) }
	| FUN xs=id_list fun_delim e=expr
			{ let rec currify e = function
				| [] -> e
				| x :: xs -> Mu (qfun (), fresh_var (), x, currify e xs)
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
	| e1=expr MOD e2=expr 
		{ Mod (e1, e2) }
	

let_pattern:
	| LET x=var EQ e1=expr IN e2=expr
			{ Let (x, e1, e2) }
	| LET f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Mu (t, f, x, e1), e2) }
	| LET f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Mu (cap t1 t2, f, x, e1), e2) }
	| LET f=var x=var EQ e1=expr IN e2=expr
			{ Let (f, Mu (qfun (), f, x, e1), e2) } 
	| LET REC f=var EQ e1=expr IN e2=expr
			{ match e1 with
				| Mu (t, _, x, e) -> Let (f, Mu (t, f, x, e), e2)
			  	| _ -> failwith "not a function" }
	| LET REC f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Mu (t, f, x, e1), e2) }
	| LET REC f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Mu (cap t1 t2, f, x, e1), e2) }
	| LET REC f=var x=var EQ e1=expr IN e2=expr
			{ Let (f, Mu (qfun (), f, x, e1), e2) } 
	| LET REC f=var EQ e1=fun_expr IN e2=expr
			{ match e1 with
			  | Mu (t, _, x, e) -> Let (f, Mu (t, f, x, e), e2)
			  | _ -> failwith "not a function" }
			
(*
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

pat_list:
	| p=PAT
		{ p }
	| p=PAT ps=pat_list
		{ p ^ ps }

pat:
	| BRACKOPEN ps=pat_list BRACKCLOSE
		{ parse_t ("[" ^ ps ^ "]") }
	| STREAM t=PAT
		{ parse_t ("Stream" ^ t) }
	| QMARK
		{ qmark () }
	| UNIT 
		{ parse_t "[]" }
	| t1=pat ARROW t2=pat
		{ mk_arrow t1 t2 }
	| t1=pat CUP t2=pat
		{ cup t1 t2 }
	| t1=pat CAP t2=pat
		{ cap t1 t2 }
	| PAROPEN t=pat PARCLOSE
		{ t }
	| BACKTICK id=IDENT
		{ parse_t ("`" ^ id) } 
	| t=PAT    			    
		{ parse_t t }   
