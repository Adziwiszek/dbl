(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Common

type cont = T.value -> T.cexp 

let tr_lit (l : S.lit) (c : cont) =
  match l with
  | S.LNum n -> c (T.Int n)
  | S.LStr s -> c (T.String s) 
  | _ -> failwith "tr_lit not implemented"

let rec tr_expr (e : S.expr) (c : cont) : T.program =
  match e with
  | S.EValue v -> tr_value v c
  | S.ELet(v, e1, e2) -> 
    tr_expr e1 (fun e1_cps ->
      let f = Var.fresh () in
      let e2_cps = (f, [v], tr_expr e2 c) in
      T.Fix([e2_cps], (T.App((T.Var f), [e1_cps]))
    ))
  | S.ELetRec _ -> failwith "tr ELetRec not implemented"
  | S.EFn(v, e) -> 
    (* Variable that this function is bound to in the continuation *)
    let f = Var.fresh () in
    (* Continuation to invoke upon function exit *)
    let k = Var.fresh () in
    (* Function f takes its original argument v along with continuation k.
       When it finishes it will bind its result to z. Finally the continuation 
       is applied to that result.
    *)
    let cps_fun = (f, [v; k], tr_expr e (fun z -> T.App(T.Var k, [z]))) in 
    T.Fix([cps_fun], c (T.Var f))
  | S.ELabel _ -> failwith "tr ELabel not impl"
  | _ -> failwith "tr_expr to cps not implemented"

and tr_value (v : S.value) (c : cont) =
  match v with
  | S.VVar v -> c (T.Var v) 
  | S.VLit l -> tr_lit l c
  | _ -> failwith "tr_value not implemented"

let init_cont v = T.Halt v

let tr_program p =
  tr_expr p init_cont
