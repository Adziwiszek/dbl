(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open SExpr
open Syntax

let tr_var x = Sym (Var.unique_name x)

let tr_value v =
  match v with
	| Var v -> tr_var v
	| Label v -> tr_var v
	| Int n -> Sym (string_of_int n)
	| String s -> Sym (Printf.sprintf "\"%s\"" (String.escaped s))

let rec tr_expr (e : cexp) : SExpr.t =
  match e with
  | Ctor _ -> tr_ctor e
	| App _ -> tr_app e
  | Fix _ -> tr_fn e
	| Switch _ -> tr_switch e
  | Halt _ -> Sym "Halt"

and tr_fn (e : cexp) : SExpr.t =
  let aux_tr_fn x : SExpr.t = 
      let (f, args, body) = x in 
      let tr_args = List.map tr_var args in
      List (Sym "fn" :: tr_var f :: tr_args @ [tr_expr body])
  in
  match e with
  | Fix(fns, c) -> 
      List ( Sym "fix" :: List.map aux_tr_fn fns @ [tr_expr c] )
  | _ -> failwith "tr_fn: Fix expected"

and tr_app e =
  match e with
  | App(v, args) -> List ( Sym "app" :: tr_value v :: List.map tr_value args)
  | _ -> failwith "tr_app: App expected"

and tr_ctor e =
  match e with
  | Ctor(n, vs, v, c) -> List ( Sym "ctor" :: tr_var v :: Num n :: List.map tr_value vs @ [tr_expr c] )
  | _ -> tr_expr e

and tr_switch e =
  let aux_tr_cls cls = 
    let (vars, e) = cls in
    List ( List.map tr_var vars @ [tr_expr e] )
  in
  match e with
  | Switch(v, cls) ->
    List ( Sym "switch" :: List.map aux_tr_cls cls )
  | _ -> tr_expr e

let tr_program = tr_expr
