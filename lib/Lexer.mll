{
    open Parser
    exception LexingError of string
}

(* regexp *)
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"
let integer = '-'?['0' - '9']['0' - '9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
| ":"       {COLON}
| ","       {COMMA}
| "->"      {ARROW}
| "+"       {PLUS}
| "-"       {MINUS}
| "*"       {TIMES}
| "<="      {MINOR_EQ}
| "<"       {MINOR}
| "!"       {NOT}
| "and"     {AND}
| "="       {ASSIGN}
| "("       {OPEN_PAR}
| ")"       {CLOSE_PAR}

| "["       {OPEN_LIST}
| "]"       {CLOSE_LIST}

| "{"       {LBRACE}
| "}"       {RBRACE}

(* KEYWORDS *)
| "globals"         {GLOBALS}
| "functions"       {FUNCTIONS}  
| "QoS"             {QOS}
| "latency"         {LATENCY}
| "cost"            {COST}
| "services"        {SERVICES}  
| "name"            {NAME}
| "params"          {PARAMS}
| "returns"         {RETURNS}
| "SLA"             {SLA}
| "precond"         {PRECOND}
| "ok-postcond"     {OK_POSTCOND}  
| "err-postcond"    {ERR_POSTCOND}

| whitespace {read lexbuf}
| integer {INT (int_of_string (Lexing.lexeme lexbuf))}
| bool {BOOL (bool_of_string (Lexing.lexeme lexbuf))}
| id {VAR (Lexing.lexeme lexbuf)}
| eof {EOF}

| _ as c { raise (LexingError (Printf.sprintf "Unexpected character '%c' at position %d" c (Lexing.lexeme_start lexbuf))) }
