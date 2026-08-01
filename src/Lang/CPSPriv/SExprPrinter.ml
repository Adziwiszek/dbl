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

let rec tr_expr (e : cexp) =
  match e with
	| Record _ -> failwith "Record sexpr not implemented" 
  | Ctor _ -> failwith "Ctor sexpr not implemented" 
	| Select _ -> failwith "Select sexpr not implemented" 
	| Offset _ -> failwith "Offset sexpr not implemented" 

	| App _ -> tr_app e

  | Fix _ -> List (Sym "fn" :: tr_fn e)

	| Switch _ -> failwith "Switch sexpr not implemented" 
	| Primop _ -> failwith "Primop sexpr not implemented" 
  | Halt _ -> failwith "Halt sexpr not implemented" 

and tr_fn e =
  match e with
  (* TODO: translate functions *)
  | Fix(fns, c) -> [ tr_expr c ]
  | _ -> [ tr_expr e ]

and tr_app e =
  match e with
  | App(v, args) -> List (tr_value v :: List.map tr_value args)
  | _ -> failwith "tr_app: App expected"

let tr_program = tr_expr
