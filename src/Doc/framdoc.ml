(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* automatyczne generowanie dokumentacji

# Lekser 
prosty lekser szukający komentarzy dokumentacji i zapamiętujący je
(powinien sobie zapamiętać do jakiej funkcji należy dany komentarz)

# odczytywanie typu itp
src/Lang/Unif.mli : 
type 'a node = {
  pos  : Position.t;
    (** Location in the source code *)

  pp   : PPTree.t;
    (** Context of the pretty-printer *)

  data : 'a
    (** Payload of the node *)
}

EffectInference usuwa informacje o położeniu z 'a node

pomysł: ukradnij env z EffectInference i z niego wyczytaj typy zmiennych


TODO:
  1) sidebar w dokumentacji
  2) dodać informacje o pozycji do ConE
*)

open Printf

let output_dir = ref "docs"

(* File handling =========================================================== *)

(** Reads file and returns its content *)
let read_file fname =
  let ic = open_in fname in
  let rec aux c buf =
    match In_channel.input_line c with
    | None -> buf
    | Some s -> aux c (buf ^ "\n" ^ s)
  in
  let content = aux ic "" in
  close_in ic; content

(** Collects recursively all .fram files in the given directory *)
let rec collect_files dir =
  let entries = Sys.readdir dir in
  Array.fold_left (fun acc entry ->
    let path = Filename.concat dir entry in
    if Sys.is_directory path then
      acc @ collect_files path
    else if Filename.check_suffix path ".fram" then
      acc @ [path]
    else
      acc
  ) [] entries

(* Html templates ========================================================== *)

(** html templates *)

let html_above = "src/Doc/html_templates/module.html"
let html_index = "src/Doc/html_templates/index.html"
let html_below = read_file "src/Doc/html_templates/foot.html"
let html_search = read_file "src/Doc/html_templates/search_page.html"

(** Replaces all occurences of sub with repl in string s *)
let replace_template s sub repl =
  let buf = Buffer.create (String.length s) in
  let slen = String.length s in
  let sublen = String.length sub in
  let i = ref 0 in
  while !i <= slen - sublen do
    if String.sub s !i sublen = sub then begin
      Buffer.add_string buf repl;
      i := !i + sublen
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  if !i < slen then Buffer.add_string buf (String.sub s !i (slen - !i));
  Buffer.contents buf

let render_html template bindings =
  List.fold_left
    (fun acc (key, value) -> replace_template acc ("{{" ^ key ^ "}}") value)
    template
    bindings

(* IR of modules =========================================================== *)

(** Single module *)

type module_doc = {
  mod_name : string;
  mod_path : string;
  mod_html : string;
  mod_entries : (string * string * int) list; (* name, doc, line *)
}

let write_module_page m =
  let fname = Filename.concat !output_dir m.mod_html in
  let oc = open_out fname in
  let html_template = read_file html_above in
  let entries_html = m.mod_entries
    |> List.map (fun (name, doc, line) ->
        if name = "" 
          then
           Printf.sprintf
             {|<div id="glob-doc" class="md-doc"> %s </div>|} doc
          else
           Printf.sprintf
             {|<div id="%s" class="def-section">
                 <h3><code>val %s</code> <span class="line">line %d</span></h3>
                 <p>%s</p>
               </div>|} name name line doc)
    |> String.concat "\n"
  in
  let module_page = Printf.sprintf "<h1>%s</h1>%s" 
    m.mod_name entries_html in
  render_html html_template ["entries", module_page] |> Printf.fprintf oc "%s";
  close_out oc

(* Filling templates ======================================================= *)

let write_index modules =
  let fname = Filename.concat !output_dir "index.html" in
  let oc = open_out fname in
  let html_template = read_file html_index in
  let links = modules
    |> List.map (fun m ->
        Printf.sprintf {|<li><a href="%s">%s</a> (%d definitions)</li>|}
          m.mod_html m.mod_name (List.length m.mod_entries))
    |> String.concat "\n"
  in
  let index_page = Printf.sprintf "<ul class=\"module-list\">%s</ul>" links in
  render_html html_template ["page", index_page] |> Printf.fprintf oc "%s";
  close_out oc

let write_doc_css output_dir =
  let fname = "src/Doc/html_templates/styles.css" in
  let css = read_file fname in
  let path = Filename.concat output_dir "styles.css" in
  let oc = open_out path in
  output_string oc css;
  close_out oc

(** Debug function for printing all docs *)
let print_docs docs = 
  printf "found %d docs\n" (List.length docs);
  List.iter (fun (name, content, line) ->
    printf "%s (line: %d)\n%s\n" name line content) docs

(* Prog arguments ========================================================== *)

let fnames : string list ref = ref []

let usage_string =
  Printf.sprintf
    "Usage: %s [OPTION]... FILE...|DIRECTORY...\nAvailable options are:"
    Sys.argv.(0)

let cmd_args_options = Arg.align
  [
  ]

let proc_arg arg =
  if Sys.is_directory arg then
    fnames := collect_files arg
  else
    fnames := [arg]

(* Main ==================================================================== *)

let _ =
  Arg.parse cmd_args_options proc_arg usage_string;
  (try Unix.mkdir !output_dir 0o755 with Unix.Unix_error _ -> ());

  (* Parse all modules with Lexer *)
  let modules = List.map (fun path ->
    let entries = Lexer.main path in
    let mod_name = path |> Filename.basename |> Filename.remove_extension in
    List.iter (fun (name, doc, line) ->
      add_entry mod_name name line doc
    )
    entries;
    { mod_name;
      mod_path = path;
      mod_html = mod_name ^ ".html";
      mod_entries = entries 
    }
  ) !fnames
  in
  (* Data for doc search engine *)
  (* write_search_data modules; *)
  (* Create index.html *)
  write_index modules;
  (* Create pages for all modules *)
  List.iter write_module_page modules;
  (* Create css styles file *)
  write_doc_css !output_dir;

  (* debugging *)
  List.map (fun m -> m.mod_entries) modules |> List.iter print_docs;

  Printf.printf "Docs written to %s/\n" !output_dir;

