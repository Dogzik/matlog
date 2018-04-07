
let (>>) x f = f x;;

let parse_string parser s = s >> Lexing.from_string >> parser Lexer.main;;
let parse_assumptions assump = parse_string Parser.assump assump;;
let parse_expression expr = parse_string Parser.main expr;;
