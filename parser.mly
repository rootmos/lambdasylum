%token LAMBDA
%token DOT
%token PLUS
%token STAR
%token LPAR
%token RPAR
%token LBR
%token RBR
%token HYPH
%token BOT
%token EXCL
%token <string> WILDCARD
%token <string> IDENTIFIER
%token <int> INT
%token EOF

%start <Parsed.program> program

%%

program:
  | t = term; EOF { t }
  ;

term:
  | t = term; EXCL { `Force t }
  | t = delimited(LPAR, inner_term, RPAR) { t }
  | t = inner_term { t }
  ;

inner_term:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = term; t2 = simple_term { `App (t1, t2) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t = simple_term { t }
  ;

simple_term:
  | i = IDENTIFIER { `Ident i }
  | i = INT { `Int i }
  | BOT { `Bottom }
  | t = delimited(LPAR, inner_term, RPAR) { t }
  | t = delimited(LBR, inner_term, RBR) { `Thunk t }
  ;

operator:
  | PLUS { `Ident "+" }
  | STAR { `Ident "*" }
  | HYPH { `Ident "-" }
  ;

pattern:
  | WILDCARD { `Wildcard }
  | i = IDENTIFIER { `Ident i }
  ;
