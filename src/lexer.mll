(* This file was copied from Tommaso's setvariants *)


{

	open Lexing
	open Parser

	exception Error of string

	let fail message = raise (Error message)

	let comment_level = ref 0

	let table = [
			(* ("match",       MATCH);
			("with",        WITH);
			("fun",         FUN);
			("rec",         REC);
			("recfun",      RECFUN);
			("function",    FUNCTION);
			("recfunction", RECFUNCTION);
			("let",         LET);
			("in",          IN);
			("true",        BOOLEAN true);
			("false",       BOOLEAN false);
			("if",          IF);
			("then",        THEN);
			("else",        ELSE);
			("as",        AS); *)
		]

	let filter id =
		try List.assoc id table
		with Not_found ->
			if id = "unit" then fail "Invalid identifier 'unit'."
			else IDENT id

}

(* Rules. *)

let newline         =  ('\010' | '\013' | "\013\010")
let blank           = [' ' '\009' '\012']
(* let decimal_literal = '-'? ['0'-'9'] ['0'-'9' '_']* *)
let ident           = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*
let pp							= '\''? ['?' '0'-'9' '"' 'A'-'Z' 'a'-'z' '_' '-' '>' '|' '&' '[' ']']+
let parpp 					= '(' pp ')'
let pat 					  = (parpp|pp)+ (' '? (parpp|pp))*
let any 						= _*

rule token = parse
	| newline
			{ let pos = lexbuf.lex_curr_p in
				lexbuf.lex_curr_p <-
				{ pos with
				    pos_lnum = pos.pos_lnum + 1;
				    pos_bol  = pos.pos_cnum;
				};
				token lexbuf }
  | blank +         { token lexbuf }
  | "(*"            { comment_level := 0; comment lexbuf; token lexbuf }
  | '.'             { DOT }
	| '%'							{ MOD }
	| '?'							{ QMARK }
	| '0'							{ PAT "0" }
	| ':'						  { COLON }
	| '{'							{ BRACEOPEN }
	| '}'							{ BRACECLOSE }
  | '\\'            { LAMBDA }
	| "fun"						{ LAMBDA }
  | '('             { PAROPEN }
  | ')'             { PARCLOSE }
  | ident as id     { IDENT id }
	| pat as p 				{ PAT p }
	| eof             { EOF }
	| _ 							{ failwith "Lexing error" }

and comment = parse
	| "*)"
			{ if !comment_level <> 0 then (decr comment_level; comment lexbuf) }
  | "(*"
      { incr comment_level; comment lexbuf }
  | eof
      { fail "Unterminated comment." }
  | newline
	  	{ let pos = lexbuf.lex_curr_p in
				lexbuf.lex_curr_p <-
				{ pos with
				    pos_lnum = pos.pos_lnum + 1;
				    pos_bol  = pos.pos_cnum;
				};
				comment lexbuf }
  | _
    	{ comment lexbuf }
