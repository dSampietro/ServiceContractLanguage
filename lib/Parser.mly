%{
    open Lang    
%}

(* tokens *)
%token COLON ARROW DOT EQ COMMA EOF 
%token PLUS MINUS TIMES LE LT GE GT NOT AND
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS SERVICES NAME PARAMS RETURNS SLA PRECOND OK_POSTCOND ERR_POSTCOND 

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%left AND
%left EQ
%left LT LE GT GE
%left PLUS MINUS
%left TIMES
%right NOT
%left DOT

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
    | OPEN_LIST gs=separated_list(COMMA, global_item) CLOSE_LIST   {gs}

global_item:
    |  id=VAR COLON t=typ  {(id,t)}


(* ---------- FUNCTIONS ---------- *)
functions:
  | OPEN_LIST fs=separated_list(COMMA, func_item) CLOSE_LIST   {fs}

func_item:
    | id=VAR COLON ft=fun_type      { {fname=id; ty=ft} }

fun_type:
    | t1=typ ARROW t2=fun_type      {TArrow(t1, t2)}
    | t=typ                         {TBase(t)}

(* ---------- QOS Def---------- *)
qos_def:
    | OPEN_LIST qs=separated_list(COMMA, qos_decl) CLOSE_LIST {qs}
    
qos_decl:
    | id=VAR COLON t=typ    {(id, t)}


(* ---------- SERVICES ---------- *)
services:
    | OPEN_LIST ss=separated_list(COMMA, service) CLOSE_LIST { ss }


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
    | OPEN_LIST p=separated_list(COMMA, param) CLOSE_LIST { p }

param:
  | id=VAR COLON t=typ { (id, t) }



returns:
    | OPEN_LIST r=separated_list(COMMA, return_item) CLOSE_LIST { r }

return_item:
    | id=VAR COLON t=typ { (id, t) }


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
  | es=separated_list(COMMA, expr) { es }


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
    | e1=expr EQ e2=expr        {EBinOp(Eq,e1,e2)}
    | NOT e=expr                    {EUnOp(Not,e)}


typ:
    | VAR {
        match $1 with
        | "int" -> TInt
        | "bool" -> TBool
        | "Outcome" -> TOutcome
        | _ -> TCustom $1
        }