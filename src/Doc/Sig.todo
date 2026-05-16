(* TODO:
  odzyskać typy z interpretera
  *)

(*
let _ = 
  "hello.fram"
  |> get_program
  |> find_definition 25
  |> ppt_definition
*)
let set_module_dirs fname =
  DblConfig.lib_search_dirs := [DblConfig.stdlib_path];
  let cur_dir = Filename.dirname fname in
  DblConfig.local_search_dirs := [cur_dir]

let get_program path =
  set_module_dirs path;
  DblParser.Main.parse_file ~use_prelude:true path
  |> TypeInference.Main.tr_program

let ppt_definition (def : Lang.Unif.expr) =
  match def.data with
  | ELetPoly(var, _, expr) -> printf "let poly\n"
  | ELetMono _ -> failwith "find definition not implemented, err = 10"
  | ELetRec e -> printf "let rec\n"
  | ERecCtx _ -> failwith "find definition not implemented, err = 12"
  | EData(_, expr) -> printf "data\n"
  | _ -> failwith "not a definition"

let check_rec_def (def : Lang.Unif.rec_def) : unit =
  printf "rec def on line %d\n" def.rd_pos.pos_start_line;
  match PPTree.lookup def.rd_pp (PP_UID def.rd_var.uid) with
  | Found s -> printf "found: %s\n" s
  | Anon(s, _) -> printf "anon: %s\n" s
  | Unbound s -> printf "unbound %s\n" s

(** Takes line number and program ast and searches for a definition with
matching line number.*)
let rec find_definition :  int -> Lang.Unif.expr -> Lang.Unif.expr = 
  fun line p ->
  if p.pos.pos_start_line = line
  then p
  else match p.data with
    | EInst _ -> failwith "find definition not implemented, err = 1"
    | ENum _ -> failwith "find definition not implemented, err = 2"
    | ENum64 _ -> failwith "find definition not implemented, err = 3"
    | EStr _ -> failwith "find definition not implemented, err = 4"
    | EChr _ -> failwith "find definition not implemented, err = 5"
    | EFn _ -> failwith "find definition not implemented, err = 6"
    | EAppPoly _ -> failwith "find definition not implemented, err = 7"
    | EAppMono _ -> failwith "find definition not implemented, err = 8"
    | ELetPoly(var, _, expr) -> 
        find_definition line expr
    | ELetMono _ -> failwith "find definition not implemented, err = 10"
    | ELetRec e -> 
        List.iter check_rec_def e.defs; 
        find_definition line e.body
    | ERecCtx _ -> failwith "find definition not implemented, err = 12"
    | EData(_, expr) -> 
        find_definition line expr
    | ETypeAlias _ -> failwith "find definition not implemented, err = 14"
    | EMatchEmpty _ -> failwith "find definition not implemented, err = 15"
    | EMatch _ -> failwith "find definition not implemented, err = 16"
    | EMatchPoly _ -> failwith "find definition not implemented, err = 17"
    | EHandle _ -> failwith "find definition not implemented, err = 18"
    | EHandler _ -> failwith "find definition not implemented, err = 19"
    | EEffect _ -> failwith "find definition not implemented, err = 20"
    | EExtern _ -> failwith "find definition not implemented, err = 21"
    | EAnnot _ -> failwith "find definition not implemented, err = 22"
    | EAnnotEff _ -> failwith "find definition not implemented, err = 23"
    | ERepl _ -> failwith "find definition not implemented, err = 24"
    | EReplExpr _ -> failwith "find definition not implemented, err = 25"


