(** Environment for storing information about variables. *)

(** Information about a variable. *)
type var_info =
  { line_num : int
  ; var_name : string 

  }

let make_var_info line name = 
  { line_num = line
  ; var_name = name 
  }

(** Type for variable map. *)
type t = var_info Var.Map.t

let empty = Var.Map.empty

let add_var_info vmap uid vinfo = Var.Map.add uid vinfo vmap

(** Debugging printing of variables stored in the map. *)
let print_var_map vmap =
  print_endline "Var map = ";
  let vlist = Var.Map.to_list vmap in
  List.iter (fun (uid, vinfo) -> 
    (Var.unique_name uid) ^ ": " ^ 
      vinfo.var_name ^ ", " ^
      (string_of_int vinfo.line_num) 
      |> print_endline)
    vlist
