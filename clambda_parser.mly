%start <Clambda_parsetree.term> program
%start <Clambda_parsetree.value> value

%%

program:
  | t = term; EOF { t }
  ;

value:
  | LAMBDA; p = pattern; DOT; t = term; EOF { `Lambda (p, t) }
  | t = delimited(LBR, inner_term, RBR); EOF { `Thunk t }
  ;

term:
  | t = term; EXCL { `Force t }
  | t = inner_term { t }
  ;

inner_term:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | t1 = term; t2 = simple_term { `App (t1, t2) }
  | t = simple_term { t }
  ;

simple_term:
  | t = simple_term; EXCL { `Force t }
  | i = IDENTIFIER { `Ident i }
  | PLUS { `Ident "+" }
  | STAR { `Ident "*" }
  | HYPH { `Ident "-" }
  | BOT { `Bottom }
  | t = delimited(LPAR, inner_term, RPAR) { t }
  | t = delimited(LBR, inner_term, RBR) { `Thunk t }
  ;

value:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | t = delimited(LBR, inner_term, RBR) { `Thunk t }
  ;

pattern:
  | WILDCARD { `Wildcard }
  | i = IDENTIFIER { `Ident i }
  ;
