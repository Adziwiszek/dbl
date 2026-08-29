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
  | Ctor of int * value list * var * cexp
  (** Fully applied constructor of ADT.
      `int`, identifier
      `value list`, converted values that this ctor was applied to
      `var`, a variable this constructor will be bound to
      `cexp`, continuation
   *)

	| App of value * value list
  (** Application of a function to its arguments. *)

	| Fix of (var * var list * cexp) list * cexp
  (** List of mutually recursive functions.
      Tuple `var * var list * cexp` represents a function:
      - `var`, a variable this function is bound to
      - `var list`, function arguments
      - `cexp`, function body
   *)

	| Switch of value * cexp list
	| Primop of primop * value list * var list * cexp list
  | Halt of value

(** Program *)
type program = cexp

(** Produce S-expression that represents given program *)
val to_sexpr : program -> SExpr.t
