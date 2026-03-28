{
  let doc_string = ref ""
  let doc_buffer = Buffer.create 256
  let doc_comments : (string * string * int) list ref = ref []

  let add_doc name lexbuf =
    let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
    let content = String.trim (Buffer.contents doc_buffer) in
    doc_comments := (name, content, line) :: !doc_comments;
    Buffer.clear doc_buffer

  let reset_doc () =
    doc_string := "";
    Buffer.clear doc_buffer;
    doc_comments := []

}

rule doc_begin = parse
  | "##" { doc_end true lexbuf }
  | "{##" { doc_end false lexbuf }
  | '\n' { Lexing.new_line lexbuf; doc_begin lexbuf }
  | eof { () }
  | _ { doc_begin lexbuf }

and doc_end one_liner = parse
  | "##}" { let_begin lexbuf}
  | '\n' {
    Lexing.new_line lexbuf;
    Buffer.add_char doc_buffer '\n';
    if one_liner 
    then let_begin lexbuf
    else doc_end one_liner lexbuf
  }
  | _ as c { 
    Buffer.add_char doc_buffer c;
    doc_end one_liner lexbuf 
  }

and let_begin = parse
  | '\n' { Lexing.new_line lexbuf; let_begin lexbuf }
  | [' ' '\t' ]+ { let_begin lexbuf }
  | "pub let" | "pub let rec" |"let" | "let ret" { 
    extract_name lexbuf
  }
  | _ { Buffer.clear doc_buffer; doc_begin lexbuf }

and extract_name = parse
  | '\n' { Lexing.new_line lexbuf; extract_name lexbuf }
  | [' ' '\t']+ { extract_name lexbuf }
  | ['a'-'z' '_' 'A'-'Z'] ['a'-'z' '_' 'A'-'Z' '0'-'9']* as name {
    add_doc name lexbuf;
    doc_begin lexbuf
  }
  | _ { Buffer.clear doc_buffer; doc_begin lexbuf }

{
  let main fname =
    reset_doc ();
    let inchan = open_in fname in 
    let lexbuf = Lexing.from_channel inchan in
    doc_begin lexbuf;
    close_in inchan;
    List.rev !doc_comments
}
    
