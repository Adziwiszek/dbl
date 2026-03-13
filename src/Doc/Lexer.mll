{
  let doc_string = ref ""
  let doc_comments : (string * string) list ref = ref []

  let print_doc_string s = 
    print_string "this is a doc comment---------\n";
    print_string s;
    print_string "------------------------------\n"
}

rule doc_begin = parse
  | "{#!" { doc_end lexbuf }
  | eof { () }
  | _ { doc_begin lexbuf }

and doc_end = parse
  | "!#}" { let_begin lexbuf}
  | _ as c { 
    doc_string := !doc_string ^ String.make 1 c; 
    doc_end lexbuf 
  }

and let_begin = parse
  | [' ' '\t' '\n']+ { let_begin lexbuf }
  | "let" | "let ret" { 
    extract_name lexbuf
  }
  | _ { failwith "missing definition for doc comment" }

and extract_name = parse
  | [' ' '\t' '\n']+ { extract_name lexbuf }
  | ['a'-'z' '_' 'A'-'Z'] ['a'-'z' '_' 'A'-'Z' '0'-'9']* as name {
    doc_comments := (name, !doc_string) :: !doc_comments;
    doc_string := "";
    doc_begin lexbuf
  }

{
  let main fname =
    let inchan = open_in fname in 
    let lexbuf = Lexing.from_channel inchan in
    doc_begin lexbuf;
    close_in inchan;
    List.rev !doc_comments
}
    
