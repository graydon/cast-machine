(* This file was copied from Tommaso's setvariants *)

%{
	open Syntax.SE_CDuce
	open Primitives
	module CD = Cduce_lib

	let get_var_pat sp =
		let rp = List.rev sp in 
		let vp = List.hd rp in 
		let tp = List.tl rp in 
		let t = parse_t (String.concat " " (List.rev tp)) 
		in (t, mk_var vp)
%}

/* Token declarations. */

%token DOT 
%token PAROPEN PARCLOSE
%token MOD FUN

(**
%token MATCH WITH FUN REC RECFUN LET IN IF THEN 
%token ELSE AS
%token RECFUNCTION
%token LET FUNCTION
**)
/* %token LEFTANGLE RIGHTANGLE */

%token <string> IDENT
%token <string> PAT 
%token EOF EOL
%token LET EQ
%token IF THEN ELSE



%nonassoc IN IDENT DOT PARCLOSE
%nonassoc MOD


%token IN "IN"


/* Starting production. */	



%start prog
%type <Syntax.SE_CDuce.prog>			   prog 
%type <Syntax.SE_CDuce.e>              expr
%%

/* Parser definition. */

prog:
	| EOL 
		{ Eol }
	| e=expr EOF 
		{ Expr e }

expr:
	| IF c=cond THEN e1=expr ELSE e2=expr
			{ Ifz (c, e1, e2) }
	| LET x=var EQ e1=expr IN e2=expr
			{ Let (x, e1, e2) }
	| PAROPEN e1=expr PARCLOSE e2=expr
			{ App (e1, e2) }
	| PAROPEN e=expr PARCLOSE
			{ e }
	| v=IDENT e2=expr 
			{ App (Var (mk_var v), e2) }
	| v=var
			{ Var v }
	| FUN t=pat x=var DOT e=expr   
			{ Lam (t, x, e) }
	| FUN x=var DOT e=expr
			{ Lam (qfun (), x, e) }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| c=pat_const
			{ Cst c }

cond:
	| e1=expr EQ e2=expr 
		{ Eq (e1, e2) }
	| e=expr
		{ e }


var:
	| v=IDENT 
			{ mk_var v }

pat_const:
	| c=PAT
	 		{ parse_cst c }

pat:
	| t=PAT    			    
		{ parse_t t }   