%token LAMBDA
%token DOT
%token <string> WILDCARD
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%type <Parsed.term> term
%type <Parsed.value> value
%type <Parsed.pattern> pattern

%start <Parsed.program> program

%%

program:
  | v = value { v }
  ;

value:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  ;

term:
  | i = IDENTIFIER { `Ident i }
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  ;

pattern:
  | WILDCARD { `Wildcard }
  | i = IDENTIFIER { `Ident i }
  ;
