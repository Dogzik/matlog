
let (>>) x f = f x;;

let parse_string parser s = s >> Lexing.from_string >> parser Lexer.main;;
let parse_assumptions assump = parse_string Parser.assump assump;;
let parse_expression expr = parse_string Parser.main expr;;

let rec concat_lists l1 l2 = match l1 with
	| []	-> l2
	| h::t	-> h::(concat_lists t l2)
;;

let split str sep = Str.split (Str.regexp sep) str;;
