%{ open Ast %}

%token <Ident.t> IDENT
%token <int> NUM
%token <string> STRING
%token <bool> BOOL
%token <Ast.gate> LGATE
%token PLUS MINUS TIMES DIV MOD
%token EQ EQEQ GT LT GE LE NEQ
%token AND OR NOT
%token FBY
%token DOT
%token PRE
%token NODE IN OUT
%token INT POLY MONO GATE
%token OP CL COMMA SEMI BROP BRCL COL LET TEL SQOP SQCL DOUBLEARROW
%token IF THEN ELSE
%token EOF
%token HASH START STOP GET

%nonassoc ifthenelse
%left FBY
%left AND
%left OR
%left EQEQ NEQ
%left GT LT GE LE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT
%nonassoc DOT SQOP

%start file
%start phrase
%type <Ast.file> file
%type <Ast.command list option> phrase

%%

span(X):
    | i = X {{ span = ($startpos, $endpos) ; node = i }}
    ;

lit_poly:
    | BROP notes = separated_list(SEMI, poly_note) BRCL { Epoly notes }
    ;

lit_mono:
    | OP note = poly_note CL { Emono note }
    ;

poly_note:
    | pitch = expr DOUBLEARROW gate = expr COMMA velo = expr
      { (pitch, gate, velo) }
    ;

expr:
    | e1 = expr FBY e2 = expr   { Ebinop (e1, Ofby, e2) }
    | e1 = expr PLUS e2 = expr  { Ebinop (e1, Oadd, e2) }
    | e1 = expr MINUS e2 = expr { Ebinop (e1, Osub, e2) }
    | e1 = expr TIMES e2 = expr { Ebinop (e1, Omul, e2) }
    | e1 = expr DIV e2 = expr   { Ebinop (e1, Odiv, e2) }
    | e1 = expr MOD e2 = expr   { Ebinop (e1, Omod, e2) }
    | e1 = expr EQEQ e2 = expr  { Ebinop (e1, Oeq,  e2) }
    | e1 = expr NEQ  e2 = expr  { Ebinop (e1, Oneq, e2) }
    | e1 = expr GT e2 = expr    { Ebinop (e1, Ogt,  e2) }
    | e1 = expr LT e2 = expr    { Ebinop (e1, Olt,  e2) }
    | e1 = expr GE e2 = expr    { Ebinop (e1, Oge,  e2) }
    | e1 = expr LE e2 = expr    { Ebinop (e1, Ole,  e2) }
    | e1 = expr AND e2 = expr   { Ebinop (e1, Oand, e2) }
    | e1 = expr OR  e2 = expr   { Ebinop (e1, Oor,  e2) }
    | e1 = expr DOT id = IDENT  { Efield (e1, id) }
    | e1 = expr SQOP e2 = expr SQCL { Eindex (e1, e2) }
    | NOT e = expr { Eunop (Onot, e) }
    | PLUS e = expr { e }
    | MINUS e = expr { Eunop (Ominus, e) }
    | PRE OP var = IDENT CL { Epre var }
    | fn = IDENT OP params = separated_list(COMMA, expr) CL { Ecall(fn, params) }
    | IF cond = expr THEN tru = expr
      ELSE fal = expr %prec ifthenelse { Eif (cond, tru, fal) }
    | OP e = expr CL { e }
    | id = IDENT { Evar id }
    | cst = NUM { Econst (Cnum cst) }
    | cst = BOOL { Econst (Cbool cst) }
    | cst = STRING { Econst (Cstr cst) }
    | cst = LGATE { Econst (Cgate cst) }
    | lit = lit_poly { lit }
    | lit = lit_mono { lit }
    ;

ty:
    | INT { Tint }
    | POLY { Tpoly }
    | MONO { Tmono }
    | GATE { Tgate }
    ;

equ_stmts:
    | IF cond = expr BROP tru = equ_block BRCL
      ELSE BROP fal = equ_block BRCL
      { Sif (cond, tru, fal, ref []) } 
    | lhs = IDENT EQ rhs = expr SEMI
      { Seq { lhs ; rhs } }
    ;

equ_block:
    | equs = list(equ_stmts) { { stmts = equs ; locals = [] } }
    ;

args:
    | name = IDENT COL ty = ty
      {{ name = name ; val_type = ty ; sig_type = Sig_local }}
    ;

node_def:
    | NODE name = IDENT
      OP ins = separated_list(COMMA, args) CL
      OP outs = separated_list(COMMA, args) CL
      EQ LET body = equ_block TEL
      {{ name = name ;
         inputs = ins ; outputs = outs ;
         body = body }}
    ;

command:
    | HASH START { Cstart }
    | HASH STOP { Cstop }
    | HASH GET id = IDENT { Cget id }
    | def = node_def { Cdef def }
    ;

file:
    | def = command EOF { [def] }
    | def = command rest = file { def :: rest }
    ;

phrase:
    | EOF { None }
    | cmds = file { Some cmds }
    ;
