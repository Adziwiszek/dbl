(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** CPS Language. It is the result of translation from Untyped language. *)

type var = Var.t

type value =
	| Var of var
	| Label of var
	| Int of int
	| Real of string
	| String of string

type accesspath =
	| OFFp of int
	| SELp of int * accesspath
	
type primop = 
  | Plus | Mult | Minus | Div
  | Lt | Lte | Gt | Gte

type cexp =
	| Record of (value * accesspath) list * var * cexp
	| Select of int * value * var * cexp
	| Offset of int * value * var * cexp 
	| App of value * value list
	| Fix of (var * var list * cexp) list * cexp
	| Switch of value * cexp list
	| Primop of primop * value list * var list * cexp list
  | Halt of value

type program = cexp
