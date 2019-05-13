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
			("rec",					REC);
			("in",          IN);
			("fun",					FUN);
			("then", 				THEN);
			("if", 			  	IF);
			("else",				ELSE);
			("pred",				PRED);
			("succ", 				SUCC);
			("fst", 				FST);
			("snd", 				SND);
			(* ("and",					AND) *)
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
let blank           = [' ']
(* let decimal_literal = '-'? ['0'-'9'] ['0'-'9' '_']* *)
let ident           = '$'? ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*
let struct		 			= ' '* ("->"|"|"|"&"|'\\'|"--"|";") ' '*
(*items: strings, atoms, typevars, intervals *)
let ppcst						= (['0'-'9' '_']|"--")+			
(* let ppxml 					= '['	(ppcst | ['[' ']' ' '])* ']'  *)
let ppvar           = ('\'') ['0'-'9' 'A'-'Z' 'a'-'b' '_']+
let ppstr 					= '"' ("\t" | "\n" | ['=' '-' '>' '<' '+' 'A'-'Z' 'a'-'z' ' ' '0'-'9' '\'' '_'])* '"'
let ppitem					= ppcst|ppvar|ppstr|"[]"|"()"
										|"Any"|"Empty"|"Int"|"Byte"
										|"Arrow"|"Char"|"Atom"|"Pair"
										|"Record"|"String"|"Latin1"
										|"Bool"|"Float"|"AnyXml"|"Bottom"
										(* |"Namespaces"|"Abstract"
										|"Caml_int"|"In_channel"
										|"Out_channel"  *)
let ppfunvar				= "Stream" ' '* '(' ppitem ')' 
(*delimiters: parenthesis, brackets, braces *)
let dopen 					= ['(' '[' '{']
let dclose 					= [')' ']' '}']
let ppitems 				= (dopen ' '* ppitem ' '* dclose) | ppitem
let ppatoms					= (dopen ppitems struct ppitems dclose)
										| (ppitems struct ppitems) | ppitems
let pat 						= ppfunvar | ppatoms | (ppatoms | (dopen+ ppatoms dclose*)) (struct (dopen* ppatoms dclose*))+
let funpat 					= ("fun"|'\\') ' '+ '(' pat ')'

rule token = parse
	| newline					{ incr_linenum lexbuf; token lexbuf }
  | blank +         { token lexbuf }
  | "(*"            { comment_level := 0; comment lexbuf; token lexbuf }
  | '.'             { DOT }
	| "mod"						{ MOD }
	| "()"						{ UNIT }
	| "->"						{ ARROW }
	| "`"							{ BACKTICK }
	| "&"							{ CAP }
	| "|"							{ CUP }
	| "["							{ BRACKOPEN }
	| "]"							{ BRACKCLOSE }
	| '*'							{ TIMES }
	| '?'							{ QMARK }
	| '+'							{ PLUS }
	| '-'							{ MINUS }
	| '='							{ EQ }
	| ','							{ COMMA }
	| ':'							{ COLON }
	| '%'							{ CAST }
  | '('             { PAROPEN }
  | ')'             { PARCLOSE }
	| '\\' 						{ FUN }
  | ";;"				    { ENDEXPR }
  | ident as id     { filter_id id }
	| pat as t     		{ PAT t }
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
