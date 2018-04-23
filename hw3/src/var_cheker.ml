open Grammar
open Printf

module Ht = Hashtbl

let var_to_ind = (Ht.create 12 : (string, int) Ht.t)
let ind_to_var = (Ht.create 12 : (int, string) Ht.t)

let rec extract_vars expr = match expr with
	| Var(name)		-> if (not (Ht.mem var_to_ind name)) then begin
						let new_ind = Ht.length var_to_ind in
						Ht.add var_to_ind name new_ind;
						Ht.add ind_to_var new_ind name;
					   end
	| Not(exp)		-> extract_vars exp
	| Bin(s, a, b)	-> begin
						extract_vars a;
						extract_vars b;
					   end
;;

let rec eval_expr expr mask = match expr with
	| Var(name)		-> mask.(Ht.find var_to_ind name)
	| Not(exp)		-> not (eval_expr exp mask)
	| Bin(op, a, b)	-> match op with
						| And	-> (eval_expr a mask) && (eval_expr b mask)
						| Or	-> (eval_expr a mask) || (eval_expr b mask)
						| Impl	-> (not(eval_expr a mask)) || (eval_expr b mask)


let bool_to_string x = if x then "И" else "Л";;

let check_expr expr cnt ic oc =
	let mask = (Array.make cnt false) in
	let rec check ind =
		if (ind = cnt) then begin
			if ((eval_expr expr mask) = false) then begin
				fprintf oc "%s" "Высказывание ложно при ";
				let var_to_string a b c = (a ^ "=" ^ (bool_to_string mask.(b)))::c in
				let tmp = (String.concat ", " (Ht.fold var_to_string var_to_ind [])) in
				fprintf oc "%s\n" tmp;
				close_out oc;
				close_in ic;
				exit 0;
			end
		end	else begin
			mask.(ind) <- false;
			check (ind + 1);
			mask.(ind) <- true;
			check (ind + 1);
		end
	in
	check 0;
;;

let full_check expr ic oc = begin
	extract_vars expr;
	check_expr expr (Ht.length var_to_ind) ic oc;
end;;


