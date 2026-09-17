(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Common

type cont = T.value -> T.cexp 

let cps_conversion_error err_msg = failwith ("CPS conversion error: " ^ err_msg)

let convert_lit (l : S.lit) : T.value =
  match l with
  | S.LNum n -> T.Int n
  | S.LStr s -> T.String s
  | S.LNum64 n -> T.Int64 n

(** Right now used to convert values for ADT constructors.
    Doesn't use continuation on the value, just rawdogs it
    into CPS
*)
let convert_value (v : S.value) : T.value =
  match v with
  | S.VVar v -> T.Var v
  | S.VLit l -> convert_lit l
  (* TODO: figure out extern in cps*)
  | S.VExtern s -> T.Int 42

let rec tr_expr (e : S.expr) (c : cont) : T.program =
  match e with
  | S.EValue v -> tr_value v c

  | S.ELet(v, e1, e2) -> 
    (* Classic trick, we turn `let x = e1 in e2` into `(fun x -> e2) e1` *)
    tr_expr e1 (fun e1_cps ->
      (* Variable for function `(fun x -> e2)` *)
      let f = Var.fresh () in
      let e2_cps = (f, [v], tr_expr e2 c) in
      (* Create `(fun x -> e2)` ad hoc and immediately apply it to e1 *)
      T.Fix([e2_cps], (T.App((T.Var f), [e1_cps])))
    )

  | S.ELetRec(fns, e)->
    let rec aux_tr_fn fns =
      match fns with
      | [] -> []
      | (v, e) :: fns' -> 
          (* Fresh function name *)
          let f = Var.fresh () in
          (* Continuation that f will receive *)
          let k = Var.fresh () in
          let f_body = tr_expr e (fun e_cps -> T.App(T.Var k, [e_cps])) in
          (f, [v; k], f_body) :: aux_tr_fn fns'
    in 
    T.Fix(aux_tr_fn fns, tr_expr e c)

  | S.EFn(v, e) -> 
    (* Variable that this function is bound to in the continuation *)
    let f = Var.fresh () in
    (* Continuation to invoke upon function exit *)
    let k = Var.fresh () in
    (* Function f takes its original argument v along with continuation k.
       When it finishes it will bind its result to e_cps. Finally the 
       continuation is applied to that result.
    *)
    let f_body = tr_expr e (fun e_cps -> T.App(T.Var k, [e_cps])) in
    T.Fix([(f, [v; k], f_body)], c (T.Var f))

  (* Function f applied to value v TODO: good description *)
  | S.EApp(f, v) ->
    (* Return address *)
    let ret_addr = Var.fresh () in
    let x = Var.fresh () in
    let ret_fun = (ret_addr, [x], c (T.Var x)) in
    let cont = tr_expr f (fun cexp_body -> 
      tr_value v (fun v_ -> T.App(cexp_body, [v_; T.Var ret_addr]))
      ) 
    in T.Fix([ret_fun], cont)

  | S.ECtor(n, values) -> 
    let v = Var.fresh() in
    (* TODO: idk if this is 100% correct way to convert values.
     Right now can't think of anything else. *)
    let converted_values = List.map convert_value values in
    T.Ctor(n, converted_values, v, c (T.Var v))

  | S.EMatch(v, clauses) -> 
    tr_value v (fun v_cps -> 
      (* Create a function k that represents rest of the computation *)
      let k = Var.fresh () in
      let x = Var.fresh () in
      let c_as_fun = (k, [x], c (T.Var x)) in
      (* Translate the clauses *)
      let tr_clause = fun (vars, e) -> 
        (**)
        (vars, tr_expr e (fun e_cps -> T.App(T.Var k, [e_cps]))) in
      let cps_clauses = List.map tr_clause clauses in

      T.Fix([c_as_fun], T.Switch(v_cps, cps_clauses))
    ) 

  (* TODO: Algebraic effects *)
  | S.ELabel _ -> failwith "label"
  | S.EShift _ -> failwith "shift"
  | S.EReset _ -> failwith "reset"
  | S.ERepl _ | S.EReplExpr _ -> cps_conversion_error "Can't translate REPL commands (ERepl | EReplExpr)"

and tr_value (v : S.value) (c : cont) =
  match v with
  | S.VVar v -> c (T.Var v) 
  | S.VLit l -> tr_lit l c
  (* TODO: figure out extern in cps*)
  | S.VExtern s -> c (T.Int 42)

and tr_lit (l : S.lit) (c : cont) =
  match l with
  | S.LNum n -> c (T.Int n)
  | S.LStr s -> c (T.String s) 
  | S.LNum64 n -> c (T.Int64 n)


let init_cont v = T.Halt v

let tr_program p =
  tr_expr p init_cont
