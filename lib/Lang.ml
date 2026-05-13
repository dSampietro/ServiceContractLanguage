
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

type aggrop =
    | Sum
    | Avg
    | Min
    | Max
    | Sorted

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
    | EAgg of aggrop * ident  

type global = ident * typ


type policy_expr =
  | PExpr of expr
  | PAgg of aggrop * string
  | PBinOp of binop * policy_expr * policy_expr
  | PUnOp of unop * policy_expr

type policy = 
    | QosFieldOp of policy_expr
    | Regex




(* QoS *)
type qos_def = (ident * typ) list
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
    policies: policy list;
    qos: qos_def;
    services: service list;
}
