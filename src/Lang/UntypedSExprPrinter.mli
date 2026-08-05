(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating the untyped language to S-expressions *)

val tr_program : Untyped.program -> SExpr.t
