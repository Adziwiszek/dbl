(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of translation from Untyped to CPS *)

(** Translate program *)
val tr_program : Lang.Untyped.program -> Lang.CPS.program

