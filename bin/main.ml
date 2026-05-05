(* main.ml *)

(*
open ServiceConfig
let parse_with_error lexbuf =
  try
    Parser.prg Lexer.read lexbuf
  with
  | Lexer.LexingError msg ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Lexing error at line %d, column %d: %s\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
      exit 1
  | MfParser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
*)

let () =
  Printf.printf "ServiceConfig Parser - OCaml version\n";
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "A config file is needed\n";
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let input_file =
    try open_in filename
    with Sys_error msg ->
      Printf.eprintf "Cannot open file: %s\n" msg;
      exit 1
  in

  let lexbuf = Lexing.from_channel input_file in
  
  try
    let _ast = (Lib.Parser.prg Lib.Lexer.read lexbuf) in
    Printf.printf "Parse successful\n";
    close_in input_file
  with
    | Lib.Parser.Error ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        let token = Lexing.lexeme lexbuf in
        Printf.eprintf "Parse error at line %d, column %d: unexpected token '%s'\n"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          token;
        exit 1

    | exn -> close_in input_file; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
    
