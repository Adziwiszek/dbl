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
  wyciągnąć html i css z pliku ml 
  jak poprawnie przeszukiwać drzewo ast
  printować to co jest w ppt
  połączyć to z lekserem
  zrobić ładną stronę
*)

open Printf

let read_file fname =
  let ic = open_in fname in
  let rec aux c buf =
    match In_channel.input_line c with
    | None -> buf
    | Some s -> aux c (buf ^ "\n" ^ s)
  in
  let content = aux ic "" in
  close_in ic; content

(** html templates *)

let html_above = read_file "src/Doc/html_templates/head.html"
let html_below = read_file "src/Doc/html_templates/foot.html"
let html_search = read_file "src/Doc/html_templates/search_page.html"

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

type module_doc = {
  mod_name : string;
  mod_path : string;
  mod_html : string;
  mod_entries : (string * string * int) list; (* name, doc, line *)
}

let output_dir = ref "docs"

let write_module_page m =
  let fname = Filename.concat !output_dir m.mod_html in
  let oc = open_out fname in
  let entries_html = m.mod_entries
    |> List.map (fun (name, doc, line) ->
        if name = "" 
          then
           Printf.sprintf
             {|<div id="glob-doc" class="glob-doc">
                 <p>%s on line %d</p>
               </div>|} doc line
          else
           Printf.sprintf
             {|<div id="%s" class="def-section">
                 <h3><code>val %s</code> <span class="line">line %d</span></h3>
                 <p>%s</p>
               </div>|} name name line doc)
    |> String.concat "\n"
  in
  Printf.fprintf oc "%s<h2>%s</h2>%s%s"
    html_above m.mod_name entries_html html_below;
  close_out oc

  (*
let write_index modules =
  let fname = Filename.concat !output_dir "index.html" in
  let oc = open_out fname in
  let links = modules
    |> List.map (fun m ->
        Printf.sprintf {|<li><a href="%s">%s</a> (%d definitions)</li>|}
          m.mod_html m.mod_name (List.length m.mod_entries))
    |> String.concat "\n"
  in
  Printf.fprintf oc "%s<ul>%s</ul>%s" html_above links html_below;
  close_out oc
  *)

let write_search_data modules =
  let fname = Filename.concat !output_dir "search_data.js" in
  let oc = open_out fname in
  let entries = modules |> List.concat_map (fun m ->
    m.mod_entries |> List.map (fun (name, doc, line) ->
      Printf.sprintf {|{"name":"%s","module":"%s","url":"%s#%s","doc":"%s"}|}
        name m.mod_name m.mod_html name (String.escaped doc)))
  in 
  Printf.fprintf oc "const SEARCH_DATA = [%s];\n"
    (String.concat "," entries);
  close_out oc

(* Typ dla jednej definicji *)
type entry = {
  e_mod  : string;
  e_name : string;
  e_kind : string;   (* "val" | "let rec" | "type" *)
  e_line : int;
  e_sig  : string;   (* typ, na razie "" *)
  e_doc  : string;
}

(* Escape znaków specjalnych JS/HTML *)
let js_escape s =
  s |> String.split_on_char '\\' |> String.concat "\\\\"
    |> String.split_on_char '"'  |> String.concat "\\\""
    |> String.split_on_char '\n' |> String.concat "\\n"

let html_escape s =
  s |> String.split_on_char '<' |> String.concat "&lt;"
    |> String.split_on_char '>' |> String.concat "&gt;"
    |> String.split_on_char '"' |> String.concat "&quot;"

(* Jeden obiekt JS dla entry *)
let entry_to_json e =
  Printf.sprintf
    {|{mod:"%s",name:"%s",kind:"%s",line:%d,sig:"%s",doc:"%s"}|}
    (js_escape e.e_mod)
    (js_escape e.e_name)
    (js_escape e.e_kind)
    e.e_line
    (js_escape e.e_sig)
    (js_escape (html_escape e.e_doc))

(* Lista modułów do sidebara *)
let sidebar_modules modules =
  modules
  |> List.map (fun m ->
       let n = List.length m.mod_entries in
       Printf.sprintf
         {|<button class="mod-btn" onclick="setMod('%s',this)">%s<span class="cnt">%d</span></button>|}
         m.mod_name m.mod_name n)
  |> String.concat "\n"

let write_doc_css output_dir =
  let fname = "src/Doc/html_templates/styles.css" in
  let css = read_file fname in
  let path = Filename.concat output_dir "styles.css" in
  let oc = open_out path in
  output_string oc css;
  close_out oc

let write_doc_html output_dir all_entries modules =
  let json_data =
    all_entries
    |> List.map entry_to_json
    |> String.concat ",\n    "
  in
  let sidebar = sidebar_modules modules in
  let total = List.length all_entries in
  let links = modules
    |> List.map (fun m ->
        Printf.sprintf {|<li><a href="%s">%s</a> (%d definitions)</li>|}
          m.mod_html m.mod_name (List.length m.mod_entries))
    |> String.concat "\n" in

  (*let html = Printf.sprintf html_search total sidebar json_data in*)
  let path = Filename.concat output_dir "search.html" in
  let oc = open_out path in
  (* output_string oc html;*)
  Printf.fprintf oc "%s<ul>%s</ul>%s" html_above links html_below;
  close_out oc

(** creates a paragraph *)
let make_paragraph : string -> string -> int -> string = fun name doc line ->
  Printf.sprintf "<pre>val %s (line: %d) : <br>\n %s\n</pre><br><hr width=\"100%%\"<br>" name line doc

let print_docs docs = 
  printf "found %d docs\n" (List.length docs);
  List.iter (fun (name, content, line) ->
    printf "%s (line: %d)\n%s\n" name line content) docs

let all_entries : entry list ref = ref []

let add_entry mod_name name line doc : unit =
  let e = 
    { e_mod = mod_name
    ; e_name = name
    ; e_kind = "val"
    ; e_line = line
    ; e_sig = "*"
    ; e_doc = doc
    }
  in
  all_entries := e :: !all_entries

let _ =
  Arg.parse cmd_args_options proc_arg usage_string;
  (try Unix.mkdir !output_dir 0o755 with Unix.Unix_error _ -> ());
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
  write_search_data modules;
  write_doc_html !output_dir !all_entries modules;
  List.iter write_module_page modules;
  (*write_index modules;*)
  write_doc_css !output_dir;
  List.map (fun m -> m.mod_entries) modules |>
  List.iter print_docs;
  Printf.printf "Docs written to %s/\n" !output_dir;
