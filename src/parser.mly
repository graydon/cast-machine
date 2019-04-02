(* This file was copied from Tommaso's setvariants *)

%{
	open Syntax.SE_CDuce
	open Primitives
	module CD = Cduce_lib
%}

/* Token declarations. */

%token DOT LAMBDA
%token PAROPEN PARCLOSE
%token BRACEOPEN BRACECLOSE
%token MOD

(**
%token MATCH WITH FUN REC RECFUN LET IN IF THEN 
%token ELSE AS
%token RECFUNCTION
%token LET FUNCTION
**)
%token COLON
%token LEFTANGLE RIGHTANGLE
%token QMARK

%token <string> IDENT
%token <string> PAT
%token EOF

/* Starting production. */

%start prog
%type <Syntax.SE_CDuce.e>			   prog 
%type <Syntax.SE_CDuce.e>              expr
%%

/* Parser definition. */

prog:
	e=expr EOF 
		{ e }

expr:
	| PAROPEN e=expr PARCLOSE
			{ e }
	| PAROPEN e1=expr PARCLOSE e2=expr
			{ App (e1, e2) }
	| v=var e2=expr 
			{ print_endline "expr2"; App (Var v, e2) }
	| v=var
			{ Var v }
	| LAMBDA BRACEOPEN t1=pat COLON t2=pat BRACECLOSE x=var DOT e=expr   
			{ Lam (t1, t2, x, e) }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| c=pat_const
			{ Cst c }

pat:
	| t=PAT   { parse_t t } 	(* todo: replace all qmarks in t with fresh grad variables to
									have more structred grad types *)
	| QMARK   { qmark () }

var:
	| v=IDENT { mk_var v }

pat_const:
	| c=PAT
	 	{ parse_cst c }