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
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input-file>\n" Sys.argv.(0);
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
    let ast = (Parser.program Lexer.read lexbuf) in
    close_in input_file
  with
    | Parser.Error ->
      close_in input_file;
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d: unexpected token\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1);
      exit 1

    | Parsing.Parse_error ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Parse error at line %d, column %d\n"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)

    | exn -> close_in input_file; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
    

  close_in input_file