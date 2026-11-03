(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Untyped language. Similarly to Core, it is in a slightly relaxed A-normal
  form. *)

type var = Var.t

type lit =
  | LNum of int
  | LNum64 of int64
  | LStr of string

type expr =
  | EValue of value
  | ELet of var * expr * expr
  | ELetRec of (var * expr) list * expr
  | EFn of var * expr
  | EApp of expr * value
  | ECtor of int * value list
  | EMatch of value * clause list
  | ELabel of var * expr
  | EShift of value * var list * var * expr
  | EReset of value * value list * expr * var * expr
  | ERepl of (unit -> expr)
  | EReplExpr of expr * string * expr

and value =
  | VLit of lit
  | VVar of var
  | VExtern of string

and clause = var list * expr

type program = expr
