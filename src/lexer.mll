{
open Parser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let special = ['_']

rule token = parse
| space+
	{ token lexbuf }
| "//"
	{ line_comment lexbuf; token lexbuf }
| "/*"
    { block_comment lexbuf; token lexbuf }
| '\n'
	{ Lexing.new_line lexbuf; token lexbuf }
| "!" { EXCLAMATION }
| "&" { AMPERSAND }
| "|" { VERTICALBAR }
| "," { COMMA }
| ";" { SEMICOLON }
| "(" { LPAR }
| ")" { RPAR }
| "[" { LBRA }
| "]" { RBRA }
| "{" { LCUR }
| "}" { RCUR }
| "+" { PLUS }
| "-" { MINUS }
| "*" { ASTERISK }
| "/" { SLASH }
| "%" { PERCENT }
| "~" { TILDE }
| "&&" { AND }
| "||" { OR }
| "==" { EQ }
| "!=" { NE }
| "<" { LT }
| "<=" { LE }
| ">" { GT }
| ">=" { GE }
| "^" { CIRCUMFLEX }
| "<<" { LSH }
| ">>" { RSH }
| "=" { ASSIGN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { RETURN }
| digit+
	{ LITERAL_INT (int_of_string (Lexing.lexeme lexbuf)) }
| (alpha | special) (alpha | special | digit)*
    { ID (Id.make (Lexing.lexeme lexbuf)) }
| eof
	{ EOF }
| _
	{ failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

and block_comment = parse
| "\n"
	{ Lexing.new_line lexbuf; block_comment lexbuf }
| "/*"
	{ block_comment lexbuf; block_comment lexbuf }
| "*/"
    { () }
| eof
	{ () }
| _
	{ block_comment lexbuf }

and line_comment = parse
| "\n"
	{ Lexing.new_line lexbuf; () }
| eof
	{ () }
| _
	{ line_comment lexbuf }
