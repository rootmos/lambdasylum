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
  | LAMBDA; p = pattern; COLON; ty = ty; DOT; t = term { `Lambda (p, ty, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = term; t2 = simple_term { `App (t1, t2) }
  | t = simple_term { t }
  ;

simple_term:
  | t = simple_term; EXCL { `Force t }
  | i = IDENTIFIER { `Ident i }
  | i = INT { `Int i }
  | h = HASH { match h with
    | 't' -> `Bool true
    | 'f' -> `Bool false
    | _ -> failwith "unrecognized hash value" (* TODO: add proper menhir error *)
  }
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

ty:
  | t1 = ty; ARROW; t2 = ty { `Fun (t1, t2) }
  | i = IDENTIFIER { match i with
    | "int" -> `Int
    | "bool" -> `Bool
    | _ -> failwith "unrecognized type" (* TODO: add proper menhir error *)
  }
  | t = delimited(LPAR, ty, RPAR) { t }
  ;
