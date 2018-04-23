open Grammar
open Axiom_cheker
open Utils

module Ht = Hashtbl

let deduce asmps proof = begin
	let exprs_inds	= (Ht.create 1337 : (expression, int) Ht.t) in
	let inds_exprs	= (Ht.create 1337 : (int, expression) Ht.t) in
	let assumps		= (Ht.create 1337 : (expression, int) Ht.t) in
	let mp			= (Ht.create 1337 : (expression, (expression * int) list) Ht.t) in
	let proved		= (Ht.create 1337 : (expression, int * int) Ht.t) in

	let get_ind expr = Ht.find exprs_inds expr in

	let rec check_axioms expr = match (check_ax expr) with
	| ind when (ind = 0)	-> None
	| _ as ind				-> Some (ind) 
	in

	let check_assumptions expr = if (Ht.mem assumps expr) then (Some(Ht.find assumps expr)) else None in

	let upd_mp expr ind = match expr with
	| Bin (Impl, a, b)	->	if (Ht.mem exprs_inds a) then Ht.replace proved b (ind, get_ind a)
							else if (Ht.mem mp a) then Ht.replace mp a ((b, ind)::(Ht.find mp a))
								 else Ht.add mp a [(b, ind)]
	| _					-> ()
	in

	let upd_old_mp expr ind = if (Ht.mem mp expr) then begin
		List.iter (fun (e, i) -> Ht.replace proved e (i, ind)) (Ht.find mp expr);
		Ht.replace mp expr [];
	end in

	let add_assumptions asmps = begin 
		let rev_asmps = List.rev asmps in
		if ((List.length rev_asmps) > 1) then begin
			let true_asmps = List.rev (List.tl rev_asmps) in
			List.iteri (fun ind expr -> Ht.add assumps expr (ind + 1)) true_asmps;
		end;
	List.hd rev_asmps
	end in

	let false_asmp = add_assumptions asmps in
	let ind = ref 1 in
	let fa_str = string_of_expression false_asmp in
	
	let asmp_or_ax expr = begin
		let expr_str = (string_of_expression expr) in
		[expr_str;
		(expr_str ^ "->(" ^ fa_str ^ "->" ^ expr_str ^ ")");
		(fa_str ^ "->" ^ expr_str)]
	end in

	let fa expr = begin
		let expr_str = (string_of_expression expr) in
		let aa = expr_str ^ "->" ^ expr_str in
		let aaa = expr_str ^ "->(" ^ aa ^ ")" in
		let aaaa = aaa ^ "->" ^ expr_str in
		[aaa;
		("(" ^ aaa ^ ")->(" ^ aaaa ^ ")->(" ^ aa ^ ")");
		("(" ^ aaaa ^ ")->(" ^ aa ^ ")");
		aaaa;
		aa]
	end in

	let mod_pon di = begin
		let j = snd (Ht.find proved di) in
		let dj = (Ht.find inds_exprs j) in
		let a_dj = (fa_str ^ "->" ^ (string_of_expression dj)) in
		let a_di = (fa_str ^ "->" ^ (string_of_expression di)) in
		let a_dj_di = (a_dj ^ "->" ^ (string_of_expression di)) in
		[("(" ^ a_dj ^ ")->(" ^ a_dj_di ^ ")->(" ^ a_di ^ ")");
		("(" ^ a_dj_di ^ ")->(" ^ a_di ^ ")");
		a_di]
	end in

	let deduce_one expr = begin
		let res = 
		if (expr = false_asmp) then fa expr
		else begin 
			match (check_axioms expr) with
			| Some (ind) -> asmp_or_ax expr 
			| None -> match (check_assumptions expr) with
					  | Some (ind) -> asmp_or_ax expr
					  | None -> mod_pon expr	
		end in
		upd_mp expr !ind;
		upd_old_mp expr !ind;
		Ht.replace exprs_inds expr !ind;
		Ht.add inds_exprs !ind expr;
		ind := !ind + 1;
		res
	end in

	let rec get_ans exprs = match exprs with
	| []	-> []
	| h::t	-> begin
				let h_p = (deduce_one h) in
				concat_lists h_p (get_ans t)
			   end;
	in
	let res = get_ans (List.map parse_expression proof) in
	res
end;;

																								
