%token ELIF ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token BANG OR AND
%token PLUS MINUS TIMES DIV MOD IDIV LEQ GEQ EQ DIF DOT ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG
%token EOF
%start file
%type unit

%%

file:
  EOF {()}
