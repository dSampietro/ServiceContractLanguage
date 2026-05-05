%{
    open Lang    
%}

(* tokens *)
%token COLON ARROW ASSIGN
%token PLUS MINUS TIMES MINOR_EQ MINOR NOT AND
%token OPEN_PAR CLOSE_PAR OPEN_LIST CLOSE_LIST LBRACE RBRACE
%token GLOBALS FUNCTIONS QOS SERVICES NAME PARAMS RETURNS SLA PRECOND QOA OK_POSTCOND ERR_POSTCOND 

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%start <Lang.program> prg

%%

prg:
    | LBRACE 
        GLOBALS g=globals
        FUNCTIONS f=functions
        SERVICES s
    RBRACE EOF

    {
        {globals = g; functions = f; services = s}
    }

(* ---------- GLOBALS ---------- *)
globals:
    | OPEN_LIST gs=global_list CLOSE_LIST   {gs}

global_list:
    | /* empty */ { [] }
    | OPEN_PAR id=VAR COLON t=typ CLOSE_PAR rest=global_list  {(id,t)::rest}


(* ---------- FUNCTIONS ---------- *)

functions:
  | OPEN_LIST fs=func_list CLOSE_LIST { fs }

func_list:
    | /* empty */ { [] }
    | OPEN_PAR id=VAR COLON ft=fun_type CLOSE_PAR rest=func_list
    {
      let (args,ret) = ft in
      { fname=id; args=args; ret=ret } :: rest
    }

fun_type:
    | OPEN_PAR ts=typ_list ARROW t=typ CLOSE_PAR { (ts,t) }

typ_list:
    | typ               {[$1]}
    | typ ts=typ_list   {$1 :: ts}


(* ---------- SERVICES ---------- *)

services:
    | OPEN_LIST ss=service_list CLOSE_LIST { ss }

service_list:
    | /* empty */                   {[]}
    | s=service rest=service_list   {s :: rest}

service:
    | OPEN_BRACE
        NAME n=VAR
        PARAMS ps=params
        RETURNS rs=returns
        SLA sl=sla
        PRECOND pre=expr_list
        QOA qos=qos_list
        OK_POSTCOND ok=expr_list
        ERR_POSTCOND err=expr_list
      CLOSE_BRACE
    
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
    | id=VAR COLON t=typ rest=param_list  { (id,t)::rest }

returns:
    | OPEN_LIST rs=ret_list CLOSE_LIST { rs }

ret_list:
    | /* empty */ { [] }
    | id=VAR COLON t=typ rest=ret_list   {(id,t)::rest}

sla:
    | OPEN_BRACE
        VAR e1=expr
        VAR e2=expr
      CLOSE_BRACE
      { {sla_latency = e1; sla_cost = e2} }


(* ---------- QoS ---------- *)
qos_list:
  | OPEN_LIST qs=qos_elems CLOSE_LIST {qs}

qos_elems:
  | /* empty */ { [] }
  | e=expr rest=qos_elems
    {(Latency e) :: rest}


(* ---------- EXPRESSIONS (prefix style) ---------- *)

expr_list:
    | OPEN_LIST es=exprs CLOSE_LIST { es }

exprs:
    | /* empty */ { [] }
    | e=expr rest=exprs { e :: rest }

expr:
    | INT                           {EInt $1}
    | BOOL                          {EBool $1}
    | VAR                           {EVar $1}

    | VAR args=exprs                {EApp($2, args)}
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