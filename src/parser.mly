%{
open S
%}

%token EOF
%token EXCLAMATION
%token VERTICALBAR
%token COMMA
%token SEMICOLON
%token LPAR
%token RPAR
%token LBRA
%token RBRA
%token LCUR
%token RCUR
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token PERCENT
%token TILDE
%token AMPERSAND
%token AND
%token OR
%token EQ
%token NE
%token LT
%token LE
%token GT
%token GE
%token CIRCUMFLEX
%token LSH
%token RSH
%token ASSIGN
%token IF
%token ELSE
%token WHILE
%token RETURN
%token <int> LITERAL_INT
%token <Id.t> ID

%left OR
%left AND
%left VERTICALBAR
%left CIRCUMFLEX
%left AMPERSAND
%left EQ NE
%left LT LE GT GE
%left LSH RSH
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%nonassoc NEG TILDE

%type <S.function_definition list> translation_unit
%start translation_unit

%%

translation_unit:
  function_definition
	{ [$1] }
| translation_unit function_definition
	{ $2::$1 }
;

function_definition:
  ID LPAR identifier_list RPAR compound_statement
	{ { name = $1; parameter_list = $3; body = $5 } }
;

identifier_list:
  /* epsilon */
	{ [] }
| ID
	{ [$1] }
| identifier_list COMMA ID
	{ $1 @ [$3] }
;

compound_statement:
  LCUR statement_list RCUR
	{ Compound $2 }
;

statement_list:
  /* epsilon */
	{ [] }
| statement
	{ [$1] }
| statement_list statement
	{ $1 @ [$2] }
;

statement:
  SEMICOLON
	{ Skip }
| expression SEMICOLON
    { Assign (Id.dummy, $1) }
| ID ASSIGN expression SEMICOLON
	{ Assign ($1, $3) }
| ASTERISK primary_expression ASSIGN expression SEMICOLON
	{ Store ($2, $4) }
| IF LPAR expression RPAR statement
	{ If ($3, $5, Skip) }
| IF LPAR expression RPAR statement ELSE statement
	{ If ($3, $5, $7) }
| WHILE LPAR expression RPAR statement
	{ While ($3, $5) }
| compound_statement
	{ $1 }
| RETURN expression SEMICOLON
	{ Return $2 }
;

expression:
  primary_expression
	{ $1 }
| expression PLUS expression
	{ Binop (Add, $1, $3) }
| expression MINUS expression
	{ Binop (Sub, $1, $3) }
| expression ASTERISK expression
	{ Binop (Mul, $1, $3) }
| expression SLASH expression
	{ Binop (Div, $1, $3) }
| expression PERCENT expression
	{ Binop (Mod, $1, $3) }
| expression AND expression
	{ Binop (And, $1, $3) }
| expression OR expression
	{ Binop (Or, $1, $3) }
| expression AMPERSAND expression
	{ Binop (BitAnd, $1, $3) }
| expression VERTICALBAR expression
	{ Binop (BitOr, $1, $3) }
| expression CIRCUMFLEX expression
	{ Binop (BitXor, $1, $3) }
| expression LT expression
	{ Binop (Lt, $1, $3) }
| expression LE expression
	{ Binop (Le, $1, $3) }
| expression GT expression
	{ Binop (Lt, $3, $1) }
| expression GE expression
	{ Binop (Le, $3, $1) }
| expression LSH expression
	{ Binop (Lsh, $1, $3) }
| expression RSH expression
	{ Binop (Rsh, $1, $3) }
| expression EQ expression
	{ Binop (Eq, $1, $3) }
| expression NE expression
	{ Uniop (Not, Binop (Eq, $1, $3)) }
;

primary_expression:
  LITERAL_INT
	{ Int $1 }
| ID
	{ Var $1 }
| ID LPAR expression_list RPAR
	{ Call ($1, $3) }
| ASTERISK primary_expression
    { Ref $2 }
| LPAR expression RPAR
	{ $2 }
| MINUS expression
	{ Uniop (Neg, $2) }
| EXCLAMATION expression
	{ Uniop (Not, $2) }
| TILDE expression
	{ Uniop (BitNot, $2) }
;

expression_list:
  /* epsilon */
	{ [] }
| expression
	{ [$1] }
| expression_list COMMA expression
	{ $1 @ [$3] }
;
