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

module UidMap = Map.Make(UID)
(** Type for variable map. *)
type t = var_info UidMap.t

let empty_var_map : t = UidMap.empty

let add_var_info vmap uid vinfo = UidMap.add uid vinfo vmap

(** Debugging printing of variables stored in the map. *)
let print_var_map vmap =
  print_endline "Var map = ";
  let vlist : (UID.t * var_info) list = UidMap.to_list vmap in
  List.iter (fun (uid, vinfo) -> 
    (UID.to_string uid) ^ ": " ^ 
      vinfo.var_name ^ ", " ^
      (string_of_int vinfo.line_num) 
      |> print_endline)
    vlist
