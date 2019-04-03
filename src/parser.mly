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
%token <string> FUNPAT

(**
%token MATCH WITH FUN REC RECFUN LET IN IF THEN 
%token ELSE AS
%token RECFUNCTION
%token LET FUNCTION
**)
%token COLON
/* %token LEFTANGLE RIGHTANGLE */

%token <string> IDENT
%token <string> PAT LETPAT
%token EOF
%token LET EQ IN


%nonassoc IN IDENT DOT PARCLOSE
%nonassoc MOD



/* Starting production. */	



%start prog
%type <Syntax.SE_CDuce.e>			   prog 
%type <Syntax.SE_CDuce.e>              expr
%%

/* Parser definition. */

prog:
	| EOF 
		{ raise Empty_Program }
	| e=expr EOF 
		{ e }

expr:
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
	| FUN tp=pat_var DOT e=expr
			{ Lam (fst tp, snd tp, e) }
	| fp=pat_fun DOT e=expr
			{ Lam (fst fp, snd fp, e) }
	| e=expr MOD t=pat
			{ Cast (e, (t, dom t)) }
	| c=pat_const
			{ Cst c }

pat_var:
	| p=PAT
		{ let sp = split p in 
		  get_var_pat sp }

pat_fun:
	| p=PAT
		{ let sp = split p in 
		  let fp = List.hd sp in 
		  match fp with
		  | "fun" -> get_var_pat (List.tl sp)
		  | _ -> begin print_endline @@ "{" ^ fp ^ "}";
					failwith "error: not a pat_fun" end
		   }

var:
	| v=IDENT 
			{ mk_var v }

pat_const:
	| c=PAT
	 		{ parse_cst c }

pat:
	| t=PAT    			    
		{ parse_t t }   