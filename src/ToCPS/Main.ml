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
    print_endline "cps let";
    (* Classic trick, we turn `let x = e1 in e2` into `(fun x -> e2) e1` *)
    tr_expr e1 (fun e1_cps ->
      (* Variable for function `(fun x -> e2)` *)
      let f = Var.fresh () in
      let e2_cps = (f, [v], tr_expr e2 c) in
      (* Create `(fun x -> e2)` ad hoc and immediately apply it to e1 *)
      T.Fix([e2_cps], (T.App((T.Var f), [e1_cps])))
    )

  | S.ELetRec _ -> failwith "letrec"

  | S.EFn(v, e) -> 
    print_endline "cps fun";
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

  | S.EApp(f, v) ->
    (* "Return address" *)
    let r = Var.fresh () in
    let x = Var.fresh () in
    let ret_fun = (r, [x], c (T.Var x)) in
    let cont = tr_expr f (fun f_ -> 
      tr_value v (fun v_ -> T.App(f_, [v_; T.Var r]))
      ) 
    in T.Fix([ret_fun], cont)

  (* TODO: "eval" values here *)
  | S.ECtor(n, values) -> 
    print_endline "cps ctor";
    T.Ctor(n, [])

  | S.EMatch(v, clauses) -> failwith "ematch"
  | S.ELabel _ -> failwith "label"
  | S.EShift _ -> failwith "shift"
  | S.EReset _ -> failwith "reset"
  | _ -> failwith "tr_expr to cps not implemented"

and tr_value (v : S.value) (c : cont) =
  match v with
  | S.VVar v -> c (T.Var v) 
  | S.VLit l -> tr_lit l c
  (* TODO: figure out extern in cps*)
  | S.VExtern s -> c (T.Int 42)

let init_cont v = T.Halt v

let tr_program p =
  tr_expr p init_cont
