ocamlc -c Lang.ml
ocamllex Lexer.mll
menhir --infer Parser.mly
ocamlc -c Parser.mli
ocamlc -c Parser.ml