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

%token DOT COLON
%token PAROPEN PARCLOSE
%token MOD FUN ARROW REC TIMES PRED SUCC

(**
%token MATCH WITH FUN REC RECFUN LET IN IF THEN 
%token ELSE AS
%token RECFUNCTION
%token LET FUNCTION
**)
/* %token LEFTANGLE RIGHTANGLE */

%token <string> IDENT
%token <string> PAT 
%token EOL ENDEXPR
%token LET EQ IN
%token IF THEN ELSE PLUS MINUS 


%nonassoc IN IDENT PARCLOSE ELSE FUN
%nonassoc MOD
%left PLUS MINUS
%left TIMES 
%nonassoc PAT PAROPEN LET IF 
%left SUCC PRED



/* Starting production. */	



%start prog
%type <Syntax.SE_CDuce.prog>			   prog 
%type <Syntax.SE_CDuce.e>              expr
%%

/* Parser definition. */

prog:
	| EOL 
		{ Eol }
	| e=expr ENDEXPR 
		{ Expr e }

expr:
	| IF c=cond THEN e1=expr ELSE e2=expr
			{ Ifz (c, e1, e2) }
	| l=let_pattern
			{ l }	
	| b=binop  
			{ b }
	| PAROPEN e1=expr PARCLOSE e2=expr
			{ App (e1, e2) }
	| PAROPEN e=expr PARCLOSE
			{ e }
	| e1=expr e2=expr
			{ App (e1, e2) }
	| v=IDENT e2=expr 
			{ App (Var (mk_var v), e2) }
	| v=var
			{ Var v }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| c=pat_const
			{ Cst c }
	| PRED e=expr 
			{ Pred (e) }
	| SUCC e=expr
			{ Succ (e) }
	| f=fun_expr
			{ f }

fun_expr:
	| FUN t=pat x=var fun_delim e=expr   
			{ Lam (t, x, e) }
	| FUN x=var fun_delim e=expr
			{ Lam (qfun (), x, e) }

binop:
	| e1=expr TIMES e2=expr 
		{ Mult (e1, e2) }
	| e1=expr PLUS e2=expr
		{ Plus (e1, e2) }
	| e1=expr MINUS e2=expr
		{ Minus (e1, e2) }
	

let_pattern:
	| LET x=var EQ e1=expr IN e2=expr
			{ Let (x, e1, e2) }
	| LET f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lam (t, x, e1), e2) }
	| LET f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lam (cap t1 t2, x, e1), e2) }
	| LET f=var x=var EQ e1=expr IN e2=expr
			{ Let (f, Lam (qfun (), x, e1), e2) } 
	| LET REC f=var COLON t=pat EQ FUN x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lamrec (t, x, e1), e2) }
	| LET REC f=var COLON t1=pat EQ FUN t2=pat  x=var fun_delim e1=expr IN e2=expr
			{ Let (f, Lamrec (cap t1 t2, x, e1), e2) }
	| LET REC f=var x=var EQ e1=expr IN e2=expr
			{ Let (f, Lamrec (qfun (), x, e1), e2) } 
	| LET REC rf=var EQ f=fun_expr IN e2=expr
			{ match f with
			  | Lam (t, x, e) -> Let (rf, Lamrec (t, x, e), e2)
			  | _ -> failwith "rec used without a function" }

fun_delim : DOT {} | ARROW {}

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
