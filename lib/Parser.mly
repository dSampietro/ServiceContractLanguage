%{
    open Lang    
%}

(* tokens *)
%token COLON ARROW ASSIGN COMMA EOF
%token PLUS MINUS TIMES MINOR_EQ MINOR NOT AND
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS SERVICES NAME PARAMS RETURNS SLA PRECOND OK_POSTCOND ERR_POSTCOND 

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%left AND
%left ASSIGN
%left MINOR MINOR_EQ
%left PLUS MINUS
%left TIMES
%right NOT

%start <Lang.program> prg

%%

prg:
    | LBRACE 
        GLOBALS   COLON  g=globals     COMMA
        FUNCTIONS COLON  f=functions   COMMA
        SERVICES  COLON  s=services  
      RBRACE EOF

    {
        {globals = g; functions = f; services = s}
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
    | id=VAR COLON ft=fun_type
    {
      let (args,ret) = ft in
      { fname=id; args=args; ret=ret }
    }

fun_type:
    | ts=typ_list ARROW t=typ { (ts,t) }

typ_list:
    | typ               {[$1]}
    | typ ARROW ts=typ_list   {$1 :: ts}



(* ---------- SERVICES ---------- *)
services:
    | OPEN_LIST ss=service_list CLOSE_LIST { ss }

service_list:
    | /* empty */                   {[]}
    | s=service COMMA rest=service_list   {s :: rest}

service:
    | LBRACE
        NAME n=VAR
        PARAMS ps=params
        RETURNS rs=returns
        SLA sl=sla
        PRECOND pre=expr_list
        QOS qos=qos_list
        OK_POSTCOND ok=expr_list
        ERR_POSTCOND err=expr_list
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
    | id=VAR COLON t=typ COMMA rest=param_list  { (id,t)::rest }


returns:
    | OPEN_LIST rs=ret_list CLOSE_LIST { rs }

ret_list:
    | /* empty */ { [] }
    | id=VAR COLON t=typ COMMA rest=ret_list   {(id,t)::rest}

sla:
    | LBRACE
        VAR COLON e1=expr COMMA
        VAR COLON e2=expr
      RBRACE
      { {sla_latency = e1; sla_cost = e2} }


(* ---------- QoS ---------- *)
qos_list:
  | OPEN_LIST qs=qos_elems CLOSE_LIST {qs}

qos_elems:
  | /* empty */ { [] }
  | e=expr COMMA rest=qos_elems    {(Latency e) :: rest}


(* ---------- EXPRESSIONS (prefix style) ---------- *)
expr_list:
    | OPEN_LIST es=exprs CLOSE_LIST { es }

exprs:
    | /* empty */ { [] }
    | e=expr COMMA rest=exprs { e :: rest }

atom:
    | n=INT                         {EInt(n)}
    | b=BOOL                        {EBool(b)}
    | v=VAR                         {EVar(v)}

ident:
    | id=VAR                        {id}

expr:
    | a=atom                        {a}
    | id=ident OPEN_PAR args=exprs CLOSE_PAR           {EApp(id, args)}
    | e1=expr PLUS e2=expr          {EBinOp(Add,e1,e2)}
    | e1=expr MINUS e2=expr         {EBinOp(Sub,e1,e2)}
    | e1=expr TIMES e2=expr         {EBinOp(Mul,e1,e2)}
    | e1=expr MINOR e2=expr         {EBinOp(Lt,e1,e2)}
    | e1=expr MINOR_EQ e2=expr      {EBinOp(Le,e1,e2)}
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