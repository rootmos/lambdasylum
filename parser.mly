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
  | LPAR; t = term; RPAR { t }
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | LPAR; t1 = term; o = operator; t2 = term; RPAR { `App (`App (o, t1), t2) }
  | LPAR; t1 = term; t2 = term; RPAR { `App (t1, t2) }
  | t1 = term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = term; t2 = term; { `App (t1, t2) }
  | i = IDENTIFIER { `Ident i }
  | i = INT { `Int i }
  ;

operator:
  | PLUS { `Ident "plus" }
  ;

pattern:
  | WILDCARD { `Wildcard }
  | i = IDENTIFIER { `Ident i }
  ;
