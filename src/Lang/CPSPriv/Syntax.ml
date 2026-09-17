
type var = Var.t

type value =
	| Var of var
	| Label of var
	| Int of int
  | Int64 of int64
	| String of string
  | Extern of string

type cexp =
  | Ctor of int * value list * var * cexp
	| App of value * value list
	| Fix of (var * var list * cexp) list * cexp
	| Switch of value * clause list
  | Halt of value

and clause = var list * cexp

type program = cexp
