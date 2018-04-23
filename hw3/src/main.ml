open Grammar;;
open Utils;;
open Var_cheker;;
open Buffer;;
open Printf;;
open Proof_getter;;
open Deducer;;
open Proofs;;

let (ic, oc) = (open_in "input.txt", open_out "output.txt");;

let (asmps, res) = parse_assumptions (input_line ic);;

let rec to_right exprs expr = match exprs with
	| []	-> expr
	| [x]	-> Bin(Impl, x, expr)
	| h::t	-> Bin(Impl, h, (to_right t expr))
;;


let moved = (to_right asmps res);;

full_check moved ic oc;;

let cnt = Ht.length var_to_ind;;
let mask = (Array.make cnt false);;

let mask_to_asmps mask ind = begin
	let to_tree i e = begin
		let var = Var(Ht.find ind_to_var i) in
		if (e) then var else Not(var)
	end in
	Array.to_list (Array.mapi to_tree (Array.sub mask 0 (ind + 1)))
end;;

let get_last lst = List.hd (List.rev lst);;

let print_list lst = List.iter (fun s -> fprintf oc "%s\n" s) lst;;


let print_asmp ass =
	let str = (String.concat "," (List.map string_of_expression ass)) in
	fprintf oc "%s|-" str;
;;

let rec get_line ind =
	if (ind = cnt) then begin
		let res = get_proof moved mask in
		(*print_endline (string_of_int ind);
		print_asmp (mask_to_asmps mask (ind - 1));
		print_list res;
		print_endline "";*)
		res
	end else begin	
		let cur_a = (Var(Ht.find ind_to_var ind)) in
		
		mask.(ind) <- true;
		let ast = (mask_to_asmps mask ind) in
		let ap = (deduce ast (get_line (ind + 1))) in
		(*print_asmp ast;
		print_list ap;
		print_endline (string_of_int ind);*)
		mask.(ind) <- false;
		let asf = (mask_to_asmps mask ind) in
		let nap = (deduce asf (get_line (ind + 1))) in
		(*print_asmp asf;
		print_list nap;
		print_endline (string_of_int ind);*)
		let comb = (exclude cur_a (Not(cur_a)) moved) in
		let res = concat_lists ap (concat_lists nap comb) in
		(*print_asmp (mask_to_asmps mask (ind - 1));
		print_list res;*)
		res
	end
;;

let print_expr expr = fprintf oc "%s\n" (string_of_expression expr);;

let almost = get_line 0;;

print_asmp asmps;;
print_expr res;;
print_list almost;;
print_list (List.map string_of_expression asmps);;


let rec to_left exprs expr = match exprs with
	| []	-> print_expr expr
	| h::t	-> begin
				print_expr (to_right t expr);
				to_left t expr;
			   end
;;

to_left asmps res;;

close_out oc;
close_in ic;
