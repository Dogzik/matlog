open Grammar
open Axiom_cheker

module Ht = Hashtbl

let exprs	= (Ht.create 1337 : (expression, int) Ht.t)
let assumps	= (Ht.create 1337 : (expression, int) Ht.t)
let mp		= (Ht.create 1337 : (expression, (expression * int) list) Ht.t)
let proved	= (Ht.create 1337 : (expression, int * int) Ht.t)

let get_ind expr = Ht.find exprs expr;;

let rec check_axioms expr = match (check_ax expr) with
	| 0 -> None
	| _ as ind -> Some (ind) 
;;

let check_assumptions expr = if (Ht.mem assumps expr) then (Some (Ht.find assumps expr)) else None;;

let check_mp expr = if (Ht.mem proved expr) then (Some (Ht.find proved expr)) else None;;

let get_annotation expr = match (check_assumptions expr) with 
	| Some (num) -> Assumption (num)
	| None -> match (check_axioms expr) with
		| Some (num) -> Axiom (num)
		| None -> match (check_mp expr) with
			| Some (a, b) -> ModusPonus (a, b)
			| None -> Proofless
;;

let upd_mp expr ind = match expr with
	| Bin (Impl, a, b) ->	
		if (Ht.mem exprs a) then Ht.replace proved b (ind, get_ind a)
		else if (Ht.mem mp a) then Ht.replace mp a ((b, ind)::(Ht.find mp a))
			 else Ht.add mp a [(b, ind)]
	| _ -> ()
;;


let upd_old_mp expr ind = 
	if (Ht.mem mp expr) then begin
		List.iter (fun (e, i) -> Ht.replace proved e (i, ind)) (Ht.find mp expr);
		Ht.replace mp expr [];
	end
;;

let process expr ind = begin
	let ans = get_annotation expr in
	upd_mp expr ind;
	upd_old_mp expr ind;
	Ht.replace exprs expr ind;
	ans
end;;
							
