%start <Flambda_parsetree.term> program

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
  | CAPITAL_LAMBDA; tv = ty_var; DOT; t = term { `TyLambda (tv, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t = term; LSB; ty = ty; RSB { `TyApp (t, ty) }
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
    | _ -> raise Error
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
  | FORALL; tv = ty_var; DOT; t = ty { `Forall (tv, t) }
  | i = IDENTIFIER { match i with
    | "int" -> `Int
    | "bool" -> `Bool
    | _ -> raise Error
  }
  | i = TY_IDENT { `TyIdent i }
  | t = delimited(LPAR, ty, RPAR) { t }
  ;

ty_var:
  | i = TY_IDENT { i }
  ;
