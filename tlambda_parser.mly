%start <Tlambda_parsetree.term> program

%%

program:
  | t = term; EOF { t }
  ;

term:
  | t = term; EXCL { `Force t }
  | t = inner_term { t }
  ;

inner_term:
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = term; t2 = simple_term { `App (t1, t2) }
  | t = simple_term { t }
  ;

simple_term:
  | t = simple_term; EXCL { `Force t }
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
  | WILDCARD; COLON; t = ty { `Wildcard t }
  | i = IDENTIFIER; COLON; t = ty { `Ident (i, t) }
  ;

ty:
  | t1 = ty; ARROW; t2 = ty { `Fun (t1, t2) }
  | i = IDENTIFIER { match i with
    | "int" -> `Int
    | "bool" -> `Bool
    | _ -> failwith "unrecognized type"
  }
  | t = delimited(LPAR, ty, RPAR) { t }
  ;
