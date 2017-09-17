%start <Tilambda_parsetree.term> program
%start <Tilambda_parsetree.ty> ty_eof

%type <Tilambda_parsetree.mono> mono

%%

program:
  | t = term; EOF { t }
  ;

term:
  | t = term; EXCL { `Force t }
  | t = inner_term { t }
  ;

inner_term:
  | LAMBDA; p = pattern; COLON; ty = ty; DOT; t = term {
    `Lambda (p, Some ty, t)
  }
  | LAMBDA; p = pattern; DOT; t = term { `Lambda (p, None, t) }
  | t1 = simple_term; o = operator; t2 = term { `App (`App (o, t1), t2) }
  | t1 = term; t2 = simple_term { `App (t1, t2) }
  | t = simple_term { t }
  ;

simple_term:
  | t = simple_term; EXCL { `Force t }
  | t = simple_term; COLON; ty = ty { `Att (t, ty) }
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

ty_eof:
  | ty = ty; EOF { ty }
  ;

ty:
  | FORALL; tv = TY_IDENT; DOT; t = ty { `Forall (tv, t) }
  | ty = mono; { (ty :> Tilambda_parsetree.ty) }
  ;

mono:
  | f = TY_IDENT; args = nonempty_list(simple_mono) { `TyFun (f, args) }
  | m = simple_mono { m }
  ;

simple_mono:
  | t1 = simple_mono; ARROW; t2 = mono { `Fun (t1, t2) }
  | i = IDENTIFIER { match i with
    | "int" -> `Int
    | "bool" -> `Bool
    | _ -> raise Error
  }
  | i = TY_IDENT { `TyVar i }
  | t = delimited(LPAR, mono, RPAR) { t }
  ;
