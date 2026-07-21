(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Syntax

let rec tr_expr (p : cexp) =
  match p with
  | _ -> failwith "tr_expr not implemented"

let tr_program = tr_expr
