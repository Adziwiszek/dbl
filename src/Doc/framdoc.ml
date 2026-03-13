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

 *)
let fname = ref None

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

let _ =
  let fname = "hello.fram" in
  let doc_comments = Lexer.main fname in
  List.iter (fun (name, c) -> 
    print_endline ("Function " ^ name ^ ":");
    print_endline c
  ) doc_comments

  (* Arg.parse cmd_args_options proc_arg usage_string;
  match !fname with
  | Some fname -> parse fname
  | None -> failwith "file name not provided" *)
