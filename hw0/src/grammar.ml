type binOp = And | Or | Impl
type expression = Var of string | Not of expression | Bin of binOp * expression * expression

let string_of_binOp = function
	| And  -> "&"
	| Or   -> "|"
	| Impl -> "->"

let rec string_of_expression = function
	| Var (name)	 -> name
	| Not (expr)	 -> "(!" ^ (string_of_expression expr) ^ ")"
	| Bin (op, x, y) -> "(" ^ (string_of_binOp op) ^ "," ^ (string_of_expression x) ^ "," ^ (string_of_expression y) ^ ")" 
