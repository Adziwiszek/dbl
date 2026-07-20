(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Common

type cont = T.value -> T.cexp 

let tr_lit (l : S.lit) (c : cont) =
  match l with
  | S.LNum n -> c (T.Int n)
  | _ -> failwith "tr_lit not implemented"

let rec tr_expr (e : S.expr) (c : cont) =
  match e with
  | S.EValue v -> tr_value v c
  | _ -> failwith "tr_expr not implemented"

and tr_value (v : S.value) (c : cont) =
  match v with
  | S.VVar v -> c (T.Var v) 
  | S.VLit l -> tr_lit l c
  | _ -> failwith "tr_value not implemented"

let init_cont v = T.Halt v

let tr_program p =
  tr_expr p init_cont
