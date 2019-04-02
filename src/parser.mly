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
/* %token LEFTANGLE RIGHTANGLE */
%token QMARK

%token <string> IDENT
%token <string> PAT
%token EOF

/* %nonassoc IDENT */
%nonassoc PARCLOSE
%left MOD

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
	| PAROPEN e1=expr PARCLOSE e2=expr
			{ App (e1, e2) }
	| PAROPEN e=expr PARCLOSE
			{ e }
	| v=var e2=expr 
			{ App (Var v, e2) }
	| v=var
			{ Var v }
	| LAMBDA BRACEOPEN t=pat  BRACECLOSE x=var DOT e=expr   
			{ Lam (t, x, e) }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| c=pat_const
			{ Cst c }

pat:
	| t=PAT    							(* todo: replace all qmarks in t with fresh gradual *)
			{ parse_t t }				(* variables to have more structured gradual types *)
	| QMARK   
			{ qmark () }

var:
	| v=IDENT 
			{ mk_var v }

pat_const:
	| c=PAT
	 		{ parse_cst c }