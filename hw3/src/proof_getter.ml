open Proofs
open Var_cheker
open Grammar
open Utils

let opt_expr expr mask = if (eval_expr expr mask) then expr else Not(expr);;

let rec get_rev_proof expr mask = 
	let bin_proof a b = begin
		let aa = (opt_expr a mask) in
		let bb = (opt_expr b mask) in
		let a_p = (get_rev_proof aa mask) in
		let b_p = (get_rev_proof bb mask) in
		let p = (List.rev (split (get_bin_proof aa bb expr) "\n")) in
		concat_lists (concat_lists p b_p) a_p
	end in

	match expr with
	| Var(name)				-> [name]
	| Not(Var(name))		-> ["(!" ^ name ^ ")"]
	| Not(Not(e))			-> begin
								let e_p = (get_rev_proof e mask) in
								let p = (List.rev (split (add_double_neg e) "\n")) in
								concat_lists p e_p
							   end
	| Not(Bin(op, a, b))	-> bin_proof a b	 
	| Bin(op, a, b)			-> bin_proof a b
;;

let get_proof expr mask = List.rev (get_rev_proof expr mask);;
