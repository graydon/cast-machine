(* This file was copied from Tommaso's setvariants *)


{

	open Lexing
	open Parser


let trim_dollar s = 
		if s = "" then failwith "empty string"
		else
		if s.[0] != '$' then s
    else String.sub s 1 (String.length s - 1)


	exception Error of string

	let fail message = raise (Error message)

	let comment_level = ref 0

	let table = [
			("let",         LET);
			("in",          IN);
			("fun",					FUN);
			("then", 				THEN);
			("if", 			  	IF);
			("else",				ELSE);
		]

	let filter_id id =
		try List.assoc id table
		with Not_found ->
			if id = "unit" then fail "Invalid identifier 'unit'."
			else IDENT id

	let filter id =
		try List.assoc id table
		with Not_found ->
			if id = "unit" then fail "Invalid identifier 'unit'."
			else PAT id

	let split = Str.split (Str.regexp " +")

	let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

(* Rules. *)

let newline         =  ('\010' | '\013' | "\013\010")
let blank           = [' ' '\009' '\012']
(* let decimal_literal = '-'? ['0'-'9'] ['0'-'9' '_']* *)
let ident           = '$'? ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*
let pp							= ['?' '0'-'9' 'A'-'Z' '_' '-' '>' '|' '&' '[' ']']+
let ppvar           = '\'' ['?' '0'-'9' 'A'-'Z' '_' '-' '>' '|' '&' '[' ']']+
let ppstr 					= '"' ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']* '"'
let parpp 					= '(' (pp|ppvar|ppstr) ')'
let allpp						= (pp|ppvar|parpp|ppstr)
let pat 					  = (allpp)+ (' '? (allpp))*
let funpat 					= ("fun"|'\\') ' '+ '(' pat ')'
let any 						= _*

rule token = parse
	| newline					{ incr_linenum lexbuf; token lexbuf }
  | blank +         { token lexbuf }
  | "(*"            { comment_level := 0; comment lexbuf; token lexbuf }
  | '.'             { DOT }
	| "->"						{ ARROW }
	| '='							{ EQ }
	| ':'							{ COLON }
	| '%'							{ MOD }
  | '('             { PAROPEN }
  | ')'             { PARCLOSE }
	| '\\' 						{ FUN }
  | ";;"				    { ENDEXPR }
  | ident as id     { filter_id id }
	| pat as t     		{  PAT t }
	| eof 						{ EOL }
	| _ 							{ print_endline @@ "lex failure: " ^ (Lexing.lexeme lexbuf); failwith "Lexing error" }


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
