(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating CPS to S-expressions *)

open Syntax

val tr_program : program -> SExpr.t
