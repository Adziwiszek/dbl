
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

