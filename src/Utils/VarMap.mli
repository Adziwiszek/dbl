(** Record with documentation information about a variable 
    (type, line number). *)
type var_info

val make_var_info : int -> string -> var_info
(** Map type from uid to var_info. *)
type t

(** Empty map for doc info *)
val empty : t

(** Adds variable info to the var map *)
val add_var_info : var_info Var.Map.t -> Var.t -> var_info -> var_info Var.Map.t
(** Debug printing of variable map. *)
val print_var_map : var_info Var.Map.t -> unit
