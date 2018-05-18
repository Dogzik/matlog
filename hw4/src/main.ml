open Printf

module Ht = Hashtbl

let add_t = (Ht.create 1337 : (int * int, int) Ht.t);;
let mul_t = (Ht.create 1337 : (int * int, int) Ht.t);;
let impl_t = (Ht.create 1337 : (int * int, int) Ht.t);;
let less_t = (Ht.create 1337 : (int * int, bool) Ht.t);;

let less a b = Ht.mem less_t (a, b);;
let comparable a b = ((less a b) || (less b a));;
let add a b = Ht.find add_t (a, b);;
let mul a b = Ht.find mul_t (a, b);;
let impl a b = Ht.find impl_t (a, b);;

let ic = open_in "input.txt";;
let oc = open_out "output.txt";;

let n = int_of_string (input_line ic);;
let g = Array.init (n + 1) (fun i -> []);;

for i = 1 to n do
	let s = input_line ic in
	g.(i) <- (List.map int_of_string (Str.split (Str.regexp " +") s));
done;;

let exit_program c = 
	close_in ic;
	close_out oc;
	exit c;
;;

let used = Array.init (n + 1) (fun i -> false);;
let rec dfs s v =
	Ht.replace less_t (s, v) true;
	used.(v) <- true;
	List.iter (fun u -> if not(used.(u)) then dfs s u) g.(v);
;;
for i = 1 to n do
	Array.fill used 1 n false;	
	dfs i i;
done;;

(*add init*)
for i = 1 to n do
	for j = i to n do
		let que = Queue.create () in
		for k = 1 to n do
			if ((less i k) && (less j k)) then begin
				if (Ht.mem add_t (i, j)) then begin
					let cur = (Ht.find add_t (i, j)) in	
					if (comparable k cur) then begin
						if (less k cur) then Ht.replace add_t (i, j) k
					end
					else begin
						Queue.add k que;
					end
				end
				else begin 
					Ht.replace add_t (i, j) k;
				end;
			end;
		done;
		if (not (Ht.mem add_t (i, j))) then begin
			fprintf oc "Операция '+' не определена: %d+%d\n" i j;
			exit_program 0;
		end;
		let cur = Ht.find add_t (i, j) in
		let que_check x = 
			if (not (less cur x)) then begin
				fprintf oc "Операция '+' не определена: %d+%d\n" i j;
				exit_program 0;
			end;
		in
		Queue.iter que_check que;
		Ht.replace add_t (j, i) cur;
	done;
done;;


(*mul init*)
for i = 1 to n do
	for j = i to n do
		let que = Queue.create () in
		for k = 1 to n do
			if ((less k i) && (less k j)) then begin
				if (Ht.mem mul_t (i, j)) then begin
					let cur = (Ht.find mul_t (i, j)) in	
					if (comparable k cur) then begin
						if (less cur k) then Ht.replace mul_t (i, j) k
					end
					else begin
						Queue.add k que;
					end
				end
				else begin 
					Ht.replace mul_t (i, j) k;
				end;
			end;
		done;
		if (not (Ht.mem mul_t (i, j))) then begin
			fprintf oc "Операция '*' не определена: %d*%d\n" i j;
			exit_program 0;
		end;
		let cur = Ht.find mul_t (i, j) in
		let que_check x = 
			if (not (less x cur)) then begin				
				fprintf oc "Операция '*' не определена: %d*%d\n" i j;
				exit_program 0;
			end;
		in
		Queue.iter que_check que;
		Ht.replace mul_t (j, i) cur;
	done;
done;;

(*check distr*)
for i = 1 to n do
	for j = 1 to n do
		for k = 1 to n do
			let x = (mul (add i j) k) in
			let y = (add (mul i k) (mul j k)) in
			if (x <> y) then begin
				fprintf oc "Нарушается дистрибутивность: %d*(%d+%d)\n" k j i;
				exit_program 0;
			end;
		done;
	done;
done;;

(*any finite is impl*)

(*impl init*)
for i = 1 to n do
	for j = 1 to n do
		for k = 1 to n do
			if (less (mul i k) j) then
				if (Ht.mem impl_t (i, j)) then begin
					let cur = (Ht.find impl_t (i, j)) in
					if (less cur k) then Ht.replace impl_t (i, j) k;
				end
				else begin
					Ht.replace impl_t (i, j) k;
				end
		done;
	done;
done;;

let zero = ref (-1);;
let one = impl 1 1;;

for i = 1 to n do
	let f = ref true in
	for j = 1 to n do
		if not(less i j) then f := false;	
	done;
	if !f then zero := i;
done;;

let neg x = impl x !zero;;

for i = 1 to n do
	if ((add i (neg i)) <> one) then begin
		fprintf oc "Не булева алгебра: %d+~%d\n" i i;
		exit_program 0;
	end;
done;;

fprintf oc "Булева алгебра\n";;
exit_program 0;;

