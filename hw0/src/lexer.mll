{
open Parser;;
}

let whitespace = [' ' '\t' '\r' '\n' '\ ']
let variable   = ['A' - 'Z']+ ['A' - 'Z' '0' - '9']*

rule main = parse
        | whitespace    { main lexbuf }
        | variable as v { VAR(v) }
        | "->"          { IMPL }
        | "&"           { AND }
        | "|"           { OR }
        | "!"           { NOT }
        | "("           { OPEN }
        | ")"           { CLOSE }
        | eof           { EOF }  
