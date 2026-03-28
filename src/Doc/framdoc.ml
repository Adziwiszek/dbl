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
  jak poprawnie przeszukiwać drzewo ast
  printować to co jest w ppt
  połączyć to z lekserem
  zrobić ładną stronę
*)

open Printf

(** head of html *)
let html_above = "
<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <title>FramDoc</title>
    <style>
        body {
            font-family: sans-serif;
            max-width: 700px;
            margin: 40px auto;
            line-height: 1.6;
        }
    </style>
    <script>
    const input = document.getElementById('search');
    input.addEventListener('input', () => {
      const q = input.value.toLowerCase();
      const results = SEARCH_DATA.filter(e =>
        e.name.toLowerCase().includes(q) ||
        e.doc.toLowerCase().includes(q)
      );
      document.getElementById('results').innerHTML =
        results.map(e =>
          `<li><a href=\"${e.url}\">${e.module}.${e.name}</a> — ${e.doc.slice(0,80)}…</li>`
        ).join('');
    });
    </script>
</head>

<body>
    <h1><a href=\"index.html\">Dokumentacja</a></h1>
    <h2><a href=\"search.html\">Wyszukiwarka</a></h2>

"
(** bottom of html *)
let html_below = "

</body>
</html>
"

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
         Printf.sprintf
           {|<section id="%s">
               <h3><code>val %s</code> <span class="line">line %d</span></h3>
               <p>%s</p>
             </section>|} name name line doc)
    |> String.concat "\n"
  in
  Printf.fprintf oc "%s<h2>%s</h2>%s%s"
    html_above m.mod_name entries_html html_below;
  close_out oc

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


let write_doc_html output_dir all_entries modules =
  let json_data =
    all_entries
    |> List.map entry_to_json
    |> String.concat ",\n    "
  in
  let sidebar = sidebar_modules modules in
  let total = List.length all_entries in

  let html = Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>FramDoc</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
.doc-shell{display:grid;grid-template-columns:200px 1fr;gap:0;border:0.5px solid var(--color-border-tertiary);border-radius:var(--border-radius-lg);overflow:hidden;min-height:500px}
.doc-sidebar{background:var(--color-background-secondary);border-right:0.5px solid var(--color-border-tertiary);padding:12px;display:flex;flex-direction:column;gap:4px}
.doc-sidebar-title{font-size:11px;font-weight:500;color:var(--color-text-tertiary);letter-spacing:.07em;text-transform:uppercase;padding:4px 6px 8px}
.mod-btn{display:flex;justify-content:space-between;align-items:center;padding:6px 8px;border-radius:var(--border-radius-md);cursor:pointer;font-size:13px;color:var(--color-text-secondary);border:none;background:none;width:100%%;text-align:left}
.mod-btn:hover{background:var(--color-background-primary);color:var(--color-text-primary)}
.mod-btn.active{background:var(--color-background-primary);border:0.5px solid var(--color-border-tertiary);color:var(--color-text-primary)}
.mod-btn .cnt{font-size:11px;color:var(--color-text-tertiary);background:var(--color-background-secondary);border:0.5px solid var(--color-border-tertiary);border-radius:10px;padding:1px 6px}
.doc-main{display:flex;flex-direction:column;overflow:hidden}
.doc-topbar{padding:12px 16px;border-bottom:0.5px solid var(--color-border-tertiary);display:flex;gap:8px;align-items:center}
.search-box{position:relative;flex:1}
.search-box svg{position:absolute;left:9px;top:50%%;transform:translateY(-50%%);width:14px;height:14px;color:var(--color-text-tertiary);pointer-events:none}
.search-box input{width:100%%;padding:7px 10px 7px 30px;font-size:13px;border:0.5px solid var(--color-border-secondary);border-radius:var(--border-radius-md);background:var(--color-background-primary);color:var(--color-text-primary)}
.filter-row{display:flex;gap:6px;align-items:center;padding:0 16px 12px;border-bottom:0.5px solid var(--color-border-tertiary)}
.tag-btn{font-size:11px;padding:3px 9px;border-radius:10px;border:0.5px solid var(--color-border-tertiary);background:none;color:var(--color-text-secondary);cursor:pointer}
.tag-btn.on{background:var(--color-background-info);border-color:var(--color-border-info);color:var(--color-text-info)}
.results-label{font-size:11px;color:var(--color-text-tertiary);margin-left:auto}
.doc-content{padding:12px 16px;overflow-y:auto;max-height:400px;display:flex;flex-direction:column;gap:8px}
.def-card{border:0.5px solid var(--color-border-tertiary);border-radius:var(--border-radius-md);padding:12px 14px;cursor:pointer;transition:border-color .1s}
.def-card:hover{border-color:var(--color-border-secondary)}
.def-card.expanded{border-color:var(--color-border-secondary);background:var(--color-background-secondary)}
.def-header{display:flex;align-items:baseline;gap:8px;flex-wrap:wrap}
.def-name{font-family:var(--font-mono);font-size:14px;font-weight:500;color:var(--color-text-primary)}
.def-mod{font-size:11px;color:var(--color-text-tertiary);background:var(--color-background-secondary);border:0.5px solid var(--color-border-tertiary);border-radius:4px;padding:1px 6px}
.def-line{font-size:11px;color:var(--color-text-tertiary);margin-left:auto}
.def-sig{font-family:var(--font-mono);font-size:12px;color:var(--color-text-secondary);margin-top:4px}
.def-doc{font-size:13px;color:var(--color-text-secondary);margin-top:8px;line-height:1.6;display:none}
.def-card.expanded .def-doc{display:block}
.hl{background:#fef3c7;border-radius:2px;padding:0 1px;color:#92400e}
.empty{padding:2rem;text-align:center;font-size:13px;color:var(--color-text-tertiary)}
.kbd{display:inline-block;font-family:var(--font-mono);font-size:10px;border:0.5px solid var(--color-border-secondary);border-radius:3px;padding:1px 4px;color:var(--color-text-tertiary)}
</style>
</head>
<body>
<h1><a href="index.html">Dokumentacja</a></h1>
<div class="doc-shell">
  <div class="doc-sidebar">
    <div class="doc-sidebar-title">Modules</div>
    <button class="mod-btn active" onclick="setMod(null,this)">
      All<span class="cnt">%d</span>
    </button>
    %s
  </div>
  <div class="doc-main">
    <div class="doc-topbar">
      <div class="search-box">
        <input id="q" placeholder="Search names and docs..." oninput="render()" />
      </div>
    </div>
    <div class="filter-row">
      <button class="tag-btn on" onclick="toggleTag('val',this)">val</button>
      <button class="tag-btn on" onclick="toggleTag('let rec',this)">let rec</button>
      <button class="tag-btn on" onclick="toggleTag('type',this)">type</button>
      <span class="results-label" id="res-count"></span>
    </div>
    <div class="doc-content" id="cards"></div>
  </div>
</div>

<script>
const DATA = [
    %s
];

let activeMod = null;
let activeTags = new Set(['val','let rec','type']);
let expanded = null;

function setMod(mod, btn) {
  activeMod = mod;
  document.querySelectorAll('.mod-btn').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  render();
}

function toggleTag(tag, btn) {
  if (activeTags.has(tag)) activeTags.delete(tag);
  else activeTags.add(tag);
  btn.classList.toggle('on', activeTags.has(tag));
  render();
}

function hl(str, q) {
  if (!q) return str;
  const re = new RegExp(`(${q.replace(/[.*+?^${}()|[\]\\]/g,'\\$&')})`, 'gi');
  return str.replace(re, '<span class="hl">$1</span>');
}

function render() {
  const q = document.getElementById('q').value.trim().toLowerCase();
  const filtered = DATA.filter(d => {
    if (activeMod && d.mod !== activeMod) return false;
    if (!activeTags.has(d.kind)) return false;
    if (q && !d.name.toLowerCase().includes(q) && !d.doc.toLowerCase().includes(q) && !d.sig.toLowerCase().includes(q)) return false;
    return true;
  });
  document.getElementById('res-count').textContent = `${filtered.length} result${filtered.length!==1?'s':''}`;
  const el = document.getElementById('cards');
  if (!filtered.length) {
    el.innerHTML = '<div class="empty">No matching definitions.</div>';
    return;
  }
  el.innerHTML = filtered.map((d,i) => {
    const isExp = expanded === d.name;
    return `<div class="def-card${isExp?' expanded':''}" onclick="toggle('${d.name}')">
      <div class="def-header">
        <span class="def-name">${hl(d.name, q)}</span>
        <span class="def-mod">${d.mod}</span>
        <span class="def-mod" style="background:none">${d.kind}</span>
        <span class="def-line">line ${d.line}</span>
      </div>
      <div class="def-sig">${hl(d.sig, q)}</div>
      <div class="def-doc">${hl(d.doc, q)}</div>
    </div>`;
  }).join('');
}

function toggle(name) {
  expanded = expanded === name ? null : name;
  render();
}

render();
</script>
</body>
</html>|} total sidebar json_data
  in
  let path = Filename.concat output_dir "search.html" in
  let oc = open_out path in
  output_string oc html;
  close_out oc

(** squeezes spaces *)
let squeeze_spaces : string -> string = fun s ->
  s |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> String.concat " "

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
  write_index modules;
  Printf.printf "Docs written to %s/\n" !output_dir;
