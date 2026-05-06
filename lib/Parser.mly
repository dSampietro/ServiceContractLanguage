%{
    open Lang    
%}

(* tokens *)
%token COLON ARROW DOT ASSIGN COMMA EOF 
%token PLUS MINUS TIMES LE LT GE GT NOT AND
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS SERVICES NAME PARAMS RETURNS SLA PRECOND OK_POSTCOND ERR_POSTCOND 

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%left DOT
%left AND
%left ASSIGN
%left LT LE GT GE
%left PLUS MINUS
%left TIMES
%right NOT

%start <Lang.program> prg

%%

prg:
    | LBRACE 
        GLOBALS   COLON  g=globals     COMMA
        FUNCTIONS COLON  f=functions   COMMA
        QOS       COLON  qos=qos_def   COMMA
        SERVICES  COLON  s=services  
      RBRACE EOF

    {
        {globals = g; functions = f; qos = qos; services = s}
    }

(* ---------- GLOBALS ---------- *)
globals:
    | OPEN_LIST gs=global_list CLOSE_LIST   {gs}

global_list:
    | /* empty */ { [] }
    |  id=VAR COLON t=typ COMMA rest=global_list  {(id,t)::rest}


(* ---------- FUNCTIONS ---------- *)
functions:
  | OPEN_LIST fs=func_list CLOSE_LIST   {fs}

func_list:
    | /* empty */ { [] }
    | f=func_item rest=func_tail  {f :: rest}

func_tail:
    | /* empty */ { [] }
    | COMMA f=func_item rest=func_tail  {f :: rest}

func_item:
    | id=VAR COLON ft=fun_type      { {fname=id; ty=ft} }

fun_type:
    | t1=typ ARROW t2=fun_type      {TArrow(t1, t2)}
    | t=typ                         {TBase(t)}

(* ---------- QOS Def---------- *)
(* TODO: make it extensible *)
qos_def:
    | OPEN_LIST qs=separated_list(COMMA, qos_decl) CLOSE_LIST {qs}
    
qos_decl:
    | id=VAR COLON t=typ    {(id, t)}


(* ---------- SERVICES ---------- *)
services:
    | OPEN_LIST ss=separated_list(COMMA, service) CLOSE_LIST { ss }

(*
service_list:
    | /* empty */                   {[]}
    | s=service COMMA rest=service_list   {s :: rest}
*)

service:
    | LBRACE
        NAME         COLON   n=VAR            COMMA
        PARAMS       COLON   ps=params        COMMA
        RETURNS      COLON   rs=returns       COMMA
        SLA          COLON   sl=sla           COMMA
        PRECOND      COLON   pre=expr_list    COMMA
        QOS          COLON   qos=qos_constr   COMMA
        OK_POSTCOND  COLON   ok=expr_list     COMMA
        ERR_POSTCOND COLON   err=expr_list
      RBRACE
    
    {
      {
        name = n;
        params = ps;
        returns = rs;
        sla = sl;
        precond = pre;
        qos = qos;
        ok_post = ok;
        err_post = err;
      }
    }

params:
    | OPEN_LIST ps=param_list CLOSE_LIST { ps }

param_list:
    | /* empty */ { [] }
    | p=param rest=param_tail  { p :: rest }

param_tail:
  | /* empty */ { [] }
  | COMMA p=param rest=param_tail { p :: rest }

param:
  | id=VAR COLON t=typ { (id, t) }



returns:
    | OPEN_LIST rs=ret_list CLOSE_LIST { rs }

ret_list:
    | /* empty */ { [] }
    | r=return rest=ret_tail   {r::rest}

ret_tail:
    | /* empty */ { [] }
    | COMMA r=return rest=ret_tail   {r::rest}
return:
    | id=VAR COLON t=typ { (id, t) }

(*TODO: unify SLA and QOS+*)
sla:
    | OPEN_LIST assigns=separated_list(COMMA, assign) CLOSE_LIST  {assigns}

assign:
    | id=VAR COLON e=expr      {(id, e)}

(* ---------- QoS ---------- *)
qos_constr:
  | OPEN_LIST 
        es=separated_list(COMMA, expr)
    CLOSE_LIST
    { es }


(* ---------- EXPRESSIONS (prefix style) ---------- *)
expr_list:
    | OPEN_LIST es=exprs CLOSE_LIST { es }

exprs:
  | /* empty */ { [] }
  | e=expr rest=exprs_tail { e :: rest }

exprs_tail:
  | /* empty */ { [] }
  | COMMA e=expr rest=exprs_tail { e :: rest }

atom:
    | n=INT                         {EInt(n)}
    | b=BOOL                        {EBool(b)}
    | v=VAR                         {EVar(v)}
    | SLA                           {ESla}

expr:
    | a=atom                        {a}
    | id=VAR OPEN_PAR args=exprs CLOSE_PAR           {EApp(id, args)}
    | e=expr DOT field=VAR          {EField(e, field)}
    | e1=expr PLUS e2=expr          {EBinOp(Add,e1,e2)}
    | e1=expr MINUS e2=expr         {EBinOp(Sub,e1,e2)}
    | e1=expr TIMES e2=expr         {EBinOp(Mul,e1,e2)}
    | e1=expr LT e2=expr            {EBinOp(Lt,e1,e2)}
    | e1=expr LE e2=expr            {EBinOp(Le,e1,e2)}
    | e1=expr GT e2=expr            {EBinOp(Gt,e1,e2)}
    | e1=expr GE e2=expr            {EBinOp(Ge,e1,e2)}
    | e1=expr AND e2=expr           {EBinOp(And,e1,e2)}
    | e1=expr ASSIGN e2=expr        {EBinOp(Eq,e1,e2)}
    | NOT e=expr                    {EUnOp(Not,e)}


typ:
    | VAR {
        match $1 with
        | "int" -> TInt
        | "bool" -> TBool
        | "Outcome" -> TOutcome
        | _ -> TCustom $1
        }