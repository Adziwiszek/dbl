(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Untyped language. Similarly to Core, it is in a slightly relaxed A-normal
  form. *)

include UntypedPriv.Syntax

let to_sexpr = UntypedPriv.SExprPrinter.tr_program
