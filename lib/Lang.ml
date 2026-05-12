
type typ = 
    | TInt
    | TBool
    | TOutcome
    | TErr
    | TArrow of typ list * typ
    | TCustom of string


type binop =
    | Add | Sub | Mul | Div
    | Lt | Le | Gt | Ge | Eq | Neq
    | Or

type unop =
    | Not

type ident = string

type expr = 
    | EInt of int
    | EBool of bool
    | EVar of ident
    | ESla
    | EField of expr * ident
    | EApp of ident * expr list
    | EBinOp of binop * expr * expr
    | EUnOp of unop * expr    

type global = ident * typ

(* QoS *)
type qos_def = (ident * typ) list

(*type qos_attr =
    | Latency of expr
    | Cost of expr
*)

type qos_constraint = expr list

(* SLA *)
type sla = (ident * expr) list

(* Parameters and returns *)
type param = ident * typ
type ret = ident * typ

(* Conditions *)
type condition = expr

type service = {
    name : ident;
    params : param list;
    returns : ret list;
    sla : sla;
    precond : condition list;
    qos : qos_constraint;
    ok_post : condition list;
    err_post : condition list;
}

(* Function signatures: * -> int *)
type funtype = 
    | TArrow of typ * funtype
    | TBase of typ

type func_sig = {
    fname : ident;
    ty : funtype;
}

type program = {
    globals: global list;
    functions: func_sig list;
    qos: qos_def;
    services: service list;
}
