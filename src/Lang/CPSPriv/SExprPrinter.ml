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
	| Real r -> Sym (Printf.sprintf "\"%s\"" (String.escaped r))
	| String s -> Sym (Printf.sprintf "\"%s\"" (String.escaped s))

let rec tr_expr (e : cexp) : SExpr.t =
  match e with
	| Record _ -> failwith "Record sexpr not implemented" 
  | Ctor _ -> tr_ctor e
	| Select _ -> failwith "Select sexpr not implemented" 
	| Offset _ -> failwith "Offset sexpr not implemented" 

	| App _ -> tr_app e

  | Fix _ -> tr_fn e

	| Switch _ -> failwith "Switch sexpr not implemented" 
	| Primop _ -> failwith "Primop sexpr not implemented" 
  | Halt _ -> failwith "Halt sexpr not implemented" 

and tr_fn (e : cexp) : SExpr.t =
  let aux_tr_fn x : SExpr.t = 
      let (f, args, body) = x in 
      let tr_args = List.map tr_var args in
      List (Sym "fn" :: tr_var f :: tr_args @ [tr_expr body])
  in
  match e with
  (* TODO: translate functions *)
  | Fix(fns, c) -> 
      List ( Sym "fix" :: List.map aux_tr_fn fns @ [tr_expr c] )
  | _ -> failwith "tr_fn: Fix expected"

and tr_app e =
  match e with
  | App(v, args) -> List ( Sym "app" :: tr_value v :: List.map tr_value args)
  | _ -> failwith "tr_app: App expected"

and tr_ctor e =
  match e with
  | Ctor(n, vs) -> List ( Sym "ctor" :: Num n :: List.map tr_value vs )
  | _ -> tr_expr e

let tr_program = tr_expr
