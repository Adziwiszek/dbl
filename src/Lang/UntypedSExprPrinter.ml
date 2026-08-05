(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating the untyped language to S-expressions *)

open SExpr
open Untyped

let tr_var x = Sym (Var.unique_name x)

let tr_lit l =
  match l with
  | LNum   n -> string_of_int n
  | LNum64 n -> Int64.to_string n ^ "L"
  | LStr   s -> Printf.sprintf "\"%s\"" (String.escaped s)

let rec tr_expr e =
  match e with
  | EValue v -> tr_value v
  | ELet(x, e1, e2) ->
    List [ Sym "let"; tr_var x; tr_expr e1; tr_expr e2 ]
  | ELetRec(rds, body) ->
    List [
      Sym "let-rec";
      List (List.map tr_rec_def rds);
      tr_expr body
    ]
  | EFn(x, body) ->
    List [ Sym "fn"; tr_var x; tr_expr body ]
  | EApp(e1, v2) ->
    List [ Sym "app"; tr_expr e1; tr_value v2 ]
  | ECtor(n, args) ->
    List (Sym "ctor" :: Num n :: List.map tr_value args)
  | EMatch(v, cls) ->
    List [
      Sym "match";
      tr_value v;
      List (Sym "clauses" :: List.map tr_clause cls)
    ]
  | ELabel(x, body) ->
    List [ Sym "label"; tr_var x; tr_expr body ]
  | EShift(v, xs, k, body) ->
    List [
      Sym "shift";
      tr_value v;
      List (List.map tr_var xs);
      tr_var k;
      tr_expr body
    ]
  | EReset(v, vs, body, x, ret) ->
    List [
      Sym "reset";
      tr_value v;
      List (List.map tr_value vs);
      tr_expr body;
      tr_var x;
      tr_expr ret
    ]
  | ERepl _ ->
    List [ Sym "repl" ]
  | EReplExpr(e1, tp, e2) ->
    List [ Sym "repl-expr"; tr_expr e1; Sym ("{" ^ tp ^ "}"); tr_expr e2 ]

and tr_value v =
  match v with
  | VLit l -> List [ Sym (tr_lit l) ]
  | VVar x -> tr_var x
  | VExtern name -> List [ Sym "extern"; Sym name ]

and tr_rec_def (x, body) =
  List [ tr_var x; tr_expr body ]

and tr_clause (xs, body) =
  List [
    List (Sym "vars" :: List.map tr_var xs);
    tr_expr body
  ]

let tr_program p = 
  print_endline "translating untyped";
  tr_expr p
