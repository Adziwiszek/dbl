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
</head>

<body>
    <h1>Dokumentacja</h1>

"
(** bottom of html *)
let html_below = "

</body>
</html>
"

let fname : string option ref = ref None

let usage_string =
  Printf.sprintf
    "Usage: %s [OPTION]... FILE [CMD_ARG]...\nAvailable OPTIONs are:"
    Sys.argv.(0)

let cmd_args_options = Arg.align
  [
  ]

let proc_arg arg =
  match !fname with
  | None -> fname := Some arg
  | Some _ -> assert false

(** squeezes spaces *)
let squeeze_spaces : string -> string = fun s ->
  s |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> String.concat " "

let set_module_dirs fname =
  DblConfig.lib_search_dirs := [DblConfig.stdlib_path];
  let cur_dir = Filename.dirname fname in
  DblConfig.local_search_dirs := [cur_dir]

let get_program path =
  set_module_dirs path;
  DblParser.Main.parse_file ~use_prelude:true path
  |> TypeInference.Main.tr_program

(** Takes line number and program ast and searches for a definition with
matching line number.*)
let find_definition :  int -> Lang.Unif.expr -> string = fun line p ->
  let correct_line = p.pos.pos_start_line = line in
  match p.data with
  | ELetMono(v, _, _) -> v.name
  | _ -> ":("


(** creates a paragraph *)
let make_paragraph : string -> string -> int -> string = fun name doc line ->
  Printf.sprintf "<pre>val %s (line: %d) : <br>\n %s\n</pre><br><hr width=\"100%%\"<br>" name line doc

let _ = 
  "hello.fram"
  |> get_program
  |> find_definition 6
  |> print_endline
(*
let _ =
  let fname = "hello.fram" in
  let outchan = open_out "hello.html" in
  let doc_comments = Lexer.main fname in
  let html_doc_comments = doc_comments 
    |> List.map (fun (name, doc, line) -> make_paragraph name (squeeze_spaces doc) line) in
  let html = String.concat "\n" (html_above :: html_doc_comments @ [html_below]) in
  Printf.fprintf outchan "%s" html; close_out outchan
*)   


  (* Arg.parse cmd_args_options proc_arg usage_string;
  match !fname with
  | Some fname -> parse fname
  | None -> failwith "file name not provided" *)
