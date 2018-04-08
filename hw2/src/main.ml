open Grammar;;
open Utils;;
open Checker;;
open Buffer;;
open Printf;;

let (ic, oc) = (open_in "input.txt", open_out "output.txt");;

let add_assumptions asmps = begin 
	let rev_asmps = List.rev asmps in
	if ((List.length rev_asmps) > 1) then begin
		let true_asmps = List.rev (List.tl rev_asmps) in
		List.iteri (fun ind expr -> Ht.add assumps expr (ind + 1)) true_asmps;
		fprintf oc "%s" (String.concat ", " (List.map string_of_expression true_asmps));
	end;
	List.hd rev_asmps
end;;

let (asmps, res) = parse_assumptions (input_line ic);;

let false_asmp = add_assumptions asmps;;

fprintf oc "%s\n" ("|-" ^ (string_of_expression (Bin (Impl, false_asmp, res))));;

let ind = ref 1;;

let fa_str = string_of_expression false_asmp;; 

let asmp_or_ax expr = begin
	let expr_str =  (string_of_expression expr) in
	fprintf oc "%s\n" expr_str;
	fprintf oc "%s\n" (expr_str ^ "->(" ^ fa_str ^ "->" ^ expr_str ^ ")");
	fprintf oc "%s\n" (fa_str ^ "->" ^ expr_str);
end;;

let fa expr = begin
	let expr_str = (string_of_expression expr) in
	let aa = expr_str ^ "->" ^ expr_str in
	let aaa = expr_str ^ "->(" ^ aa ^ ")" in
	let aaaa = aaa ^ "->" ^ expr_str in
	fprintf oc "%s\n" aaa;
	fprintf oc "%s\n" ("(" ^ aaa ^ ")->(" ^ aaaa ^ ")->(" ^ aa ^ ")");
	fprintf oc "%s\n" ("(" ^ aaaa ^ ")->(" ^ aa ^ ")");
	fprintf oc "%s\n" aaaa;
	fprintf oc "%s\n" aa;
end;;

let mod_pon di = begin
	let j = snd (Ht.find proved di) in
	let dj = (Ht.find inds_exprs j) in
	let a_dj = (fa_str ^ "->" ^ (string_of_expression dj)) in
	let a_di = (fa_str ^ "->" ^ (string_of_expression di)) in
	let a_dj_di = (a_dj ^ "->" ^ (string_of_expression di)) in
	fprintf oc "%s\n" ("(" ^ a_dj ^ ")->(" ^ a_dj_di ^ ")->(" ^ a_di ^ ")");
	fprintf oc "%s\n" ("(" ^ a_dj_di ^ ")->(" ^ a_di ^ ")");
	fprintf oc "%s\n" a_di;
end;;

try
	while true do
		let line = input_line ic in
		if (line <> "") then begin
			let expr = parse_expression line in
			if (expr = false_asmp) then fa expr
			else begin
				match (check_axioms expr) with
				| Some (ind) -> asmp_or_ax expr 
				| None -> match (check_assumptions expr) with
					| Some (ind) -> asmp_or_ax expr
					| None -> mod_pon expr
			end;
			upd_mp expr !ind;
			upd_old_mp expr !ind;
			Ht.replace exprs_inds expr !ind;
			Ht.add inds_exprs !ind expr;
			ind := !ind + 1;
		end;
	done;
with End_of_file -> close_out oc;
					close_in ic;
