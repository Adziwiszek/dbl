(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)

open Common
open TypeCheckFix

(** Module of mutually recursive functions of the type-checker.
  See [TypeCheckFix] for more details. *)
module rec TCFix : TCFix = struct
  let infer_expr_type ?app_type env e =
    Expr.infer_expr_type ~tcfix:(module TCFix) ?app_type env e

  let check_expr_type env e tp =
    Expr.check_expr_type ~tcfix:(module TCFix) env e tp

  let check_def env def tp_req cont =
    Def.check_def ~tcfix:(module TCFix) env def tp_req cont

  let check_defs env defs tp_req cont =
    Def.check_defs ~tcfix:(module TCFix) env defs tp_req cont
end

(* todo dodac mape z identyfikatorow w dane o wiazaniu *)

let tr_program ?var_map p =
  let env_init = (Env.initial var_map) in
  let er = TCFix.check_expr_type env_init p T.Type.t_unit in
  ConstrSolve.solve_all er.er_constr;
  InterpLib.Error.assert_no_error ();
  print_endline "type inference";
  Option.iter (fun x -> VarMap.print_var_map !x) var_map;
  er.er_expr
