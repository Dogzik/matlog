type binOp = And | Or | Impl;;
type expression = Var of string | Not of expression | Bin of binOp * expression * expression;;
type annotation = Assumption of int | Axiom of int | ModusPonus of int * int | Proofless;;

let string_of_binOp = function
	| And  -> "&"
	| Or   -> "|"
	| Impl -> "->"
;;

let rec string_of_expression = function
	| Var (name)	 -> name
	| Not (expr)	 -> "(!" ^ (string_of_expression expr) ^ ")"
	| Bin (op, x, y) -> "(" ^ (string_of_expression x) ^ (string_of_binOp op) ^ (string_of_expression y) ^ ")" 
;;

let string_of_annotation = function 
	| Assumption (ind)  -> "(Предп. " ^ (string_of_int ind) ^ ")"
	| Axiom (ind)       -> "(Сх. акс. " ^ (string_of_int ind) ^ ")"
	| ModusPonus (a, b) -> "(M.P. " ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
	| Proofless         -> "(Не доказано)"
;;
