
(* IR of a single entry ==================================================== *)

(** # Definition entries in a module *)

let js_escape s =
  s |> String.split_on_char '\\' |> String.concat "\\\\"
    |> String.split_on_char '"'  |> String.concat "\\\""
    |> String.split_on_char '\n' |> String.concat "\\n"

let html_escape s =
  s |> String.split_on_char '<' |> String.concat "&lt;"
    |> String.split_on_char '>' |> String.concat "&gt;"
    |> String.split_on_char '"' |> String.concat "&quot;"

type entry = {
  e_mod  : string;
  e_name : string;
  e_kind : string;   (* "val" | "let rec" | "type" *)
  e_line : int;
  e_sig  : string;   (* typ, na razie "" *)
  e_doc  : string;
}

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

let entry_to_json e =
  Printf.sprintf
    {|{mod:"%s",name:"%s",kind:"%s",line:%d,sig:"%s",doc:"%s"}|}
    (js_escape e.e_mod)
    (js_escape e.e_name)
    (js_escape e.e_kind)
    e.e_line
    (js_escape e.e_sig)
    (js_escape (html_escape e.e_doc))

(** Writes search data for documentation *)
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

let write_search_page_html output_dir all_entries modules =
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

(* Lista modułów do sidebara *)
let sidebar_modules modules =
  modules
  |> List.map (fun m ->
       let n = List.length m.mod_entries in
       Printf.sprintf
         {|<button class="mod-btn" onclick="setMod('%s',this)">%s<span class="cnt">%d</span></button>|}
         m.mod_name m.mod_name n)
  |> String.concat "\n"

