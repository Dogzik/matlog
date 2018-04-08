open Grammar
open Axiom_cheker

module Ht = Hashtbl

let exprs_inds	= (Ht.create 1337 : (expression, int) Ht.t)
let inds_exprs	= (Ht.create 1337 : (int, expression) Ht.t)
let assumps		= (Ht.create 1337 : (expression, int) Ht.t)
let mp			= (Ht.create 1337 : (expression, (expression * int) list) Ht.t)
let proved		= (Ht.create 1337 : (expression, int * int) Ht.t)

let get_ind expr = Ht.find exprs_inds expr;;

let rec check_axioms expr = match (check_ax expr) with
	| 0 -> None
	| _ as ind -> Some (ind) 
;;

let check_assumptions expr = if (Ht.mem assumps expr) then (Some (Ht.find assumps expr)) else None;;

let upd_mp expr ind = match expr with
	| Bin (Impl, a, b) ->	
		if (Ht.mem exprs_inds a) then Ht.replace proved b (ind, get_ind a)
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

							
