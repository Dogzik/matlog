open Grammar
open Utils

let check_ax expr =
	let check45 a b c = if (a = c) then 4 else if (b = c) then 5 else 0 in
	let check67 a b c = if (a = b) then 6 else if (a = c) then 7 else 0 in
	match expr with
	| Bin (Impl, a1, Bin (Impl, b1, a2)) 
	when (a1 = a2) -> 1
	| Bin (Impl, Bin (Impl, a1, b1), Bin (Impl, Bin (Impl, a2, Bin (Impl, b2, c1)), Bin (Impl, a3, c2)))
	when (a1 = a2 && a2 = a3 && b1 = b2 && c1 = c2) -> 2
	| Bin (Impl, a1, Bin (Impl, b1, Bin (And, a2, b2)))
	when (a1 = a2 && b1 = b2) -> 3
	| Bin (Impl, Bin (And, a, b), c) -> check45 a b c
	| Bin (Impl, a, Bin (Or, b, c)) -> check67 a b c
	| Bin (Impl, Bin (Impl, a1, c1), Bin (Impl, Bin (Impl, b1, c2), Bin (Impl, Bin (Or, a2, b2), c3)))
	when (a1 = a2 && b1 = b2 && c1 = c2 && c2 = c3) -> 8
	| Bin (Impl, Bin (Impl, a1, b1), Bin (Impl, Bin(Impl, a2, Not (b2)), Not (a3))) 
	when (a1 = a2 && a2 = a3 && b1 = b2) -> 9
	| Bin (Impl, Not (Not (a1)), a2)
	when (a1 = a2) -> 10
	| _ -> 0
;;
