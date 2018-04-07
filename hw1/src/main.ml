open Grammar;;
open Utils;;
open Checker;;
open Buffer;;
open Printf;;

let add_assumptions asmps = List.iteri (fun ind expr -> Ht.add assumps expr (ind + 1)) asmps;;

let (ic, oc) = (open_in "input.txt", open_out "output.txt");;

add_assumptions (parse_assumptions (input_line ic));;

let ind = ref 1;;

try
	while true; do
		let line = input_line ic in
		if (line <> "") then begin
			let expr = parse_expression line in
			let ann = process expr !ind in
			fprintf oc "(%d) %s %s\n" !ind (string_of_expression expr) (string_of_annotation ann);
			ind := !ind + 1;
		end
	done;
with End_of_file -> close_out oc;
					close_in ic;
