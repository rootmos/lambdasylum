%token LAMBDA
%token DOT
%token PLUS
%token LPAR
%token RPAR
%token <string> WILDCARD
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%start <Parsed.program> program

%%

program:
  | v = value; EOF { v }
  ;

value:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  ;

term:
  | LPAR; t = inner_term; RPAR; { t }
  | t = inner_term;  { t }
  ;

inner_term:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = simple_term; t2 = term { `App (t1, t2) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = simple_term; t2 = term; { `App (t1, t2) }
  | t = simple_term; { t }
  ;

simple_term:
  | i = IDENTIFIER { `Ident i }
  | i = INT { `Int i }
  | LPAR; t = inner_term; RPAR { t }
  ;

operator:
  | PLUS { `Ident "plus" }
  ;

pattern:
  | WILDCARD { `Wildcard }
  | i = IDENTIFIER { `Ident i }
  ;