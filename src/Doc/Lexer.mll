{
  let doc_buffer = Buffer.create 256
  let doc_comments : (string * string * int) list ref = ref []

  let add_doc name lexbuf =
    let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
    let content = String.trim (Buffer.contents doc_buffer) in
    doc_comments := (name, content, line) :: !doc_comments;
    Buffer.clear doc_buffer

  let print_buffer () =
    let content = String.trim (Buffer.contents doc_buffer) in
    print_endline content

  let reset_doc () =
    Buffer.clear doc_buffer;
    doc_comments := []

}

rule doc_begin = parse
  | "##" { doc_end true lexbuf }
  | "{##" { doc_end false lexbuf }
  | '\n' { Lexing.new_line lexbuf; doc_begin lexbuf }
  | eof { () }
  (* todo: łapanie definicji bez komentarza *)
  | "pub let" | "pub let rec" | "let " | "let ret" { 
    print_endline "let glob ----"; 
    extract_name lexbuf
  }
  | "parameter" {
    extract_name lexbuf
  }
  | "method" | "pub method" {
    print_endline "method glob----"; 
    extract_name lexbuf
  }
  | "module" | "pub module" {
    print_endline "module glob----"; 
    extract_name lexbuf
  }
  | "data" | "pub data" | "abstr data"  {
    print_endline "data doc glob----"; 
    extract_name lexbuf
  }
  | _ { doc_begin lexbuf }

and doc_end one_liner = parse
  | "##}\n" { def_begin lexbuf}
  | '\n' {
    Lexing.new_line lexbuf;
    Buffer.add_char doc_buffer '\n';
    if one_liner 
    then def_begin lexbuf
    else doc_end one_liner lexbuf
  }
  | _ as c { 
    Buffer.add_char doc_buffer c;
    doc_end one_liner lexbuf 
  }

and def_begin = parse
  | '\n' { Lexing.new_line lexbuf; def_begin lexbuf }
  | [' ' '\t' ]+ { def_begin lexbuf }
  | "pub let" | "pub let rec" | "let" | "let ret" { 
    print_endline "let ----"; 
    extract_name lexbuf
  }
  | "parameter" {
    extract_name lexbuf
  }
  | "method" | "pub method" {
    print_endline "method ----"; 
    extract_name lexbuf
  }
  | "module" | "pub module" {
    print_endline "module ----"; 
    extract_name lexbuf
  }
  | "data" | "pub data" | "abstr data"  {
    print_endline "data doc ----"; 
    extract_name lexbuf
  }
  | _ { 
    print_buffer ();
    add_doc "" lexbuf;
    doc_begin lexbuf 
  }

and extract_name = parse
  | '\n' { Lexing.new_line lexbuf; extract_name lexbuf }
  | [' ' '\t']+ { extract_name lexbuf }
  | ['a'-'z' '_' 'A'-'Z'] ['a'-'z' '_' 'A'-'Z' '0'-'9']* as name {
    print_endline name;
    add_doc name lexbuf;
    doc_begin lexbuf
  }
  | _ { 
    Buffer.clear doc_buffer; 
    doc_begin lexbuf 
  }

{
  let main fname =
    reset_doc ();
    let inchan = open_in fname in 
    let lexbuf = Lexing.from_channel inchan in
    doc_begin lexbuf;
    close_in inchan;
    List.rev !doc_comments
}
    
