(** Documentation lexer *)

{
let doc_buffer = Buffer.create 256

let print_buffer () =
  let content = String.trim (Buffer.contents doc_buffer) in
  print_endline content

type pending =
  | NoPending
  (* Starting line *)
  | PendingLine of int

let pending : pending ref = ref NoPending
let doc_comments : (string * string * int) list ref = ref []

let getline lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum 

let start_doc lexbuf = 
  match !pending with
  | NoPending -> pending := PendingLine (getline lexbuf)
  | PendingLine _ -> 
      print_buffer ();
      failwith "A doc comment is already pending"

let add_doc name lexbuf =
  let line = getline lexbuf in
  let content = String.trim (Buffer.contents doc_buffer) in
  doc_comments := (name, content, line) :: !doc_comments;
  Buffer.clear doc_buffer

let reset_doc () =
  Buffer.clear doc_buffer;
  doc_comments := []

}

rule main_rule = parse
  (* Documentation comments *)
  | "{##" {  gather_block_doc lexbuf }
  | "##" {  gather_line_doc lexbuf }

  (* Normal comments => skip *)
  | "{#" { skip_comment true lexbuf; main_rule lexbuf }
  | '#' { skip_comment false lexbuf; main_rule lexbuf }

  (* Definitions without documentation comments *)
  | "parameter" 

  | "abstr data"  
  | "pub data" 
  | "data" 

  | "pub method" 
  | "method" 

  | "pub module" 
  | "module" 

  | "pub let rec" 
  | "pub let" 
  | "let rec" 
  | "let " { extract_name lexbuf }

  (* Other *)
  | '\n' { Lexing.new_line lexbuf; main_rule lexbuf }
  | _ { main_rule lexbuf }
  | eof { () }

and skip_comment is_block_comment = parse
  | "#}" {
    if is_block_comment
    then ()
    else skip_comment false lexbuf
  }
  | '\n' {
    Lexing.new_line lexbuf;
    if is_block_comment
    then skip_comment true lexbuf
    else ()
  }
  | _ { skip_comment is_block_comment lexbuf }
  | eof { () }

and gather_line_doc = parse
  | '\n' {
    Lexing.new_line lexbuf;
    Buffer.add_char doc_buffer '\n';
    start_definition lexbuf
  }
  | _ as c { 
    Buffer.add_char doc_buffer c;
    gather_line_doc lexbuf 
  }

and gather_block_doc = parse
  | "##}" {
    skip_to_next_line lexbuf;
    start_definition lexbuf 
  }
  | '\n' {
    Lexing.new_line lexbuf;
    Buffer.add_char doc_buffer '\n';
    gather_block_doc lexbuf 
  }
  | _ as c { 
    Buffer.add_char doc_buffer c;
    gather_block_doc lexbuf 
  }

and start_definition = parse
  (* Definition needs to be directly under doc comment *)
  | '\n' { 
    Lexing.new_line lexbuf; 
    add_doc "" lexbuf;
    main_rule lexbuf 
  }

  | "parameter" 

  | "abstr data"  
  | "pub data" 
  | "data" 

  | "pub method" 
  | "method" 

  | "pub module" 
  | "module" 

  | "pub let rec" 
  | "pub let" 
  | "let rec" 
  | "let " { extract_name lexbuf }

  | _ { 
    print_endline "unknown definition";
    add_doc "" lexbuf;
    main_rule lexbuf 
  }

and extract_name = parse
  (* Previous comment didn't have a definition *)
  | "{##" { add_doc "" lexbuf; gather_block_doc lexbuf }
  | "##"  { add_doc "" lexbuf; gather_line_doc lexbuf }

  | "{#"  { skip_comment true lexbuf; main_rule lexbuf }
  | '#'   { skip_comment false lexbuf; main_rule lexbuf }

  (* Skip any whitespace *)
  | [' ' '\t']+ { extract_name lexbuf }

  (* Extract the name of the definition *)
  | ['a'-'z' '_' 'A'-'Z'] ['a'-'z' '_' 'A'-'Z' '0'-'9']* as name {
    add_doc name lexbuf;
    main_rule lexbuf
  }

  (* Other *)
  | _ { 
    Buffer.clear doc_buffer; 
    main_rule lexbuf 
  }

and skip_to_next_line = parse
  | '\n' { Lexing.new_line lexbuf }
  | _ { skip_to_next_line lexbuf }


{
let main fname =
  reset_doc ();
  let inchan = open_in fname in 
  let lexbuf = Lexing.from_channel inchan in
  main_rule lexbuf;
  close_in inchan;
  List.rev !doc_comments
}

