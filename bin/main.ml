(* main.ml *)

let previous_token = ref ""
let current_token = ref ""

let lexer_with_history lexbuf =
  previous_token := !current_token;
  let tok = Lib.Lexer.read lexbuf in
  current_token := Lexing.lexeme lexbuf;
  tok


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
    let ast = (Lib.Parser.prg lexer_with_history lexbuf) in
    Printf.printf "Parse successful\n";
    let fmt = Format.std_formatter in
    Lib.Lang_pp.pp_program fmt ast;
    Format.pp_print_flush fmt ();
    close_in input_file
  with
    | Lib.Parser.Error ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        let _token = Lexing.lexeme lexbuf in
        Printf.eprintf 
          "Parse error at line %d, column %d\n\
           previous token: '%s'\n\
           current token: '%s'\n"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          !previous_token
          !current_token;
        exit 1

    | exn -> close_in input_file; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
    
