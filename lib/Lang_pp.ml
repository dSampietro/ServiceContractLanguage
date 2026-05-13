open Format
open Lang

let rec pp_typ fmt = function
  | Lang.TInt -> fprintf fmt "int"
  | Lang.TBool -> fprintf fmt "bool"
  | Lang.TOutcome -> fprintf fmt "outcome"
  | Lang.TErr -> fprintf fmt "err"
  | Lang.TCustom s -> fprintf fmt "%s" s
  | Lang.TArrow (args, ret) ->
      fprintf fmt "(";
      pp_typ_list fmt args;
      fprintf fmt ") -> ";
      pp_typ fmt ret

and pp_typ_list fmt = function
  | [] -> ()
  | [t] -> pp_typ fmt t
  | t :: ts ->
      pp_typ fmt t;
      fprintf fmt ", ";
      pp_typ_list fmt ts

let pp_binop fmt = function
  | Lang.Add -> fprintf fmt "+"
  | Lang.Sub -> fprintf fmt "-"
  | Lang.Mul -> fprintf fmt "*"
  | Lang.Div -> fprintf fmt "/"
  | Lang.Lt  -> fprintf fmt "<"
  | Lang.Le  -> fprintf fmt "<="
  | Lang.Gt  -> fprintf fmt ">"
  | Lang.Ge  -> fprintf fmt ">="
  | Lang.Eq  -> fprintf fmt "=="
  | Lang.Neq -> fprintf fmt "!="
  | Lang.Or  -> fprintf fmt "||"

let pp_unop fmt = function
  | Lang.Not -> fprintf fmt "!"

let pp_aggrop fmt = function
  | Lang.Sum    -> fprintf fmt "sum"
  | Lang.Avg    -> fprintf fmt "avg"
  | Lang.Min    -> fprintf fmt "min"
  | Lang.Max    -> fprintf fmt "max"
  | Lang.Sorted -> fprintf fmt "sorted"

let rec pp_expr fmt = function
  | Lang.EInt i -> fprintf fmt "%d" i
  | Lang.EBool b -> fprintf fmt "%b" b
  | Lang.EVar v -> fprintf fmt "%s" v
  | Lang.ESla -> fprintf fmt "<sla>"
  | Lang.EField (e, id) ->
      fprintf fmt "%a.%s" pp_expr e id
  | Lang.EApp (f, args) ->
      fprintf fmt "%s(" f;
      pp_expr_list fmt args;
      fprintf fmt ")"
  | Lang.EBinOp (op, a, b) ->
      fprintf fmt "(";
      pp_expr fmt a;
      fprintf fmt " %a " pp_binop op;
      pp_expr fmt b;
      fprintf fmt ")"
  | Lang.EUnOp (op, e) ->
      fprintf fmt "(%a%a)" pp_unop op pp_expr e

and pp_expr_list fmt = function
  | [] -> ()
  | [e] -> pp_expr fmt e
  | e :: es ->
      pp_expr fmt e;
      fprintf fmt ", ";
      pp_expr_list fmt es

let pp_global fmt (id, ty) =
  fprintf fmt "%s : %a" id pp_typ ty

let pp_qos_def fmt (id, ty) =
  fprintf fmt "%s : %a" id pp_typ ty

let pp_param fmt (id, ty) =
  fprintf fmt "%s : %a" id pp_typ ty

let pp_ret fmt (id, ty) =
  fprintf fmt "%s : %a" id pp_typ ty

let pp_condition fmt = pp_expr fmt

let pp_sla fmt (id, e) =
  fprintf fmt "%s = %a" id pp_expr e

let pp_funtype =
  let rec go fmt = function
    | Lang.TBase t -> pp_typ fmt t
    | Lang.TArrow (t, rest) ->
        fprintf fmt "%a -> %a" pp_typ t go rest
  in
  go

let pp_func_sig fmt f =
  fprintf fmt "function %s : %a" f.fname pp_funtype f.ty

let rec pp_policy_expr fmt = function
  | Lang.PExpr e ->
      pp_expr fmt e

  | Lang.PAgg (agg, id) ->
      fprintf fmt "%a(%s)" pp_aggrop agg id

  | Lang.PBinOp (op, a, b) ->
      fprintf fmt "(";
      pp_policy_expr fmt a;
      fprintf fmt " %a " pp_binop op;
      pp_policy_expr fmt b;
      fprintf fmt ")"

  | Lang.PUnOp (op, e) ->
      fprintf fmt "(%a%a)" pp_unop op pp_policy_expr e

let rec pp_regex fmt = function
  | Lang.RService s ->
      fprintf fmt "%s" s

  | Lang.RConcat (r1, r2) ->
      fprintf fmt "(";
      pp_regex fmt r1;
      fprintf fmt " . ";
      pp_regex fmt r2;
      fprintf fmt ")"

  | Lang.RChoice (r1, r2) ->
      fprintf fmt "(";
      pp_regex fmt r1;
      fprintf fmt " + ";
      pp_regex fmt r2;
      fprintf fmt ")"

  | Lang.RStar r ->
      fprintf fmt "(";
      pp_regex fmt r;
      fprintf fmt ")*"

let pp_policy fmt = function
  | Lang.QosFieldOp p ->
      pp_policy_expr fmt p

  | Lang.Regex r ->
      pp_regex fmt r
    
let pp_service fmt s =
  fprintf fmt "service %s {\n" s.name;
  fprintf fmt "  params:\n";
  List.iter (fun p -> fprintf fmt "    %a\n" pp_param p) s.params;
  fprintf fmt "  returns:\n";
  List.iter (fun r -> fprintf fmt "    %a\n" pp_ret r) s.returns;
  fprintf fmt "  sla:\n";
  List.iter (fun sl -> fprintf fmt "    %a\n" pp_sla sl) s.sla;
  fprintf fmt "  precond:\n";
  List.iter (fun c -> fprintf fmt "    %a\n" pp_condition c) s.precond;
  fprintf fmt "  qos:\n";
  List.iter (fun q -> fprintf fmt "    %a\n" pp_expr q) s.qos;
  fprintf fmt "  ok_post:\n";
  List.iter (fun c -> fprintf fmt "    %a\n" pp_condition c) s.ok_post;
  fprintf fmt "  err_post:\n";
  List.iter (fun c -> fprintf fmt "    %a\n" pp_condition c) s.err_post;
  fprintf fmt "}\n"

let pp_program fmt p =
  fprintf fmt "globals:\n";
  List.iter (fun g -> fprintf fmt "  %a\n" pp_global g) p.globals;

  fprintf fmt "\nfunctions:\n";
  List.iter (fun f -> fprintf fmt "  %a\n" pp_func_sig f) p.functions;

  fprintf fmt "\nqos:\n";
  List.iter (fun q -> fprintf fmt "  %a\n" pp_qos_def q) p.qos;

  fprintf fmt "\npolicies:\n";
  List.iter (fun p -> fprintf fmt "  %a\n" pp_policy p) p.policies;

  fprintf fmt "\nservices:\n";
  List.iter (fun s -> fprintf fmt "%a\n" pp_service s) p.services