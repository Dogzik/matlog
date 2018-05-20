(*Раздобыли два алкаша бытылку метилового спирта. Выпили по первой. 
- Вань, давай сразу по второй, а то уже вечереть начинает...*)

(*Идут две зубочистки по лесу. Мимо пробегает ежик.
 Одна другой и говрит:"А я и не знала,что тут автобусы ходят"*)

(*Идет мужик с работы. На пути встречается старуха. Старуха протягивает ему свернутую в несколько раз бумагу и настоятельно так произносит: «Сам не читай- дай другим прочитать!». Мужик приходит домой, рассказывает жене, что встретил старуху которая дала ему записку, но настоятельно просила: «Сам не читай- дай другим прочитать!» Жена взяла записку, развернула ее и сказала: «Да за такие слова я больше жить с тобой не буду!» И выгнала мужа из дома.

Мужик пошел к лучшему другу проситься на ночлег. Приходит и говорит, что жена выгнала из дома. Тот удивляется: за что? Мужик рассказывает, как встретил старуху, которая дала ему какую-то записку и сказала: «Сам не читай- дай другим прочитать!» Жена прочла и выгнала его. Ну тут друг попросил посмотреть записку, чтобы разобраться. Но как только прочел ее, так злобно произнес: «Да после таких слов я тебе больше никогда не буду другом!» И выгнал мужика.

Идет мужчик по улице. Встречается ему милиционер, интересуется, почему тот в полном одиночестве идет поздно по улице. А мужик рассказывает, что встретил старуху, которая дала ему какую-то записку и сказала: «Сам не читай- дай другим прочитать!». Пришел показал жене, а та из дома выгнала. Пришел к лучшему другу, тот тоже прочел записку и тоже выставил его за порог. Милиционер заинтересовался и попросил дать ему эту записку. Прочел и возмутился: «Да за такие слова тебя судить надо!»

В день суда судья просит мужика объяснить, что произошло. Мужик вспоминает, как встретил бабку, которая дала ему записку и сказала: «Сам не читай- дай другим прочитать!» Дал почитать ее жене, а та из дома выгнала. Друг прочел- отказался от него! Потом встретился милиционер, которому тоже стало интересно, что же написано в этой записке. Прочел, после чего мужик оказался здесь, в суде. Судья так заинтересовался тем, что могло быть в записке, что попросил показать ему ее. Прочтя содержимое он произнес: «Да за такие слова тебя расстрелять мало!». Мужика расстреляли.

Попал мужик на Тот свет. А там апостол Петр встречает его и спрашивает, что, мол произошло? Ну мужик и давай рассказывать, как встретил старуху, которая дала ему записку и строго наказала: «Сам не читай- дай другим прочитать!» Дал жене почитать, а та из дома выгнала. Дал другу почитать, а тот от него отрекся. Милиционер прочел и под суд отдал. А судья после прочтенного приговорил к расстрелу. Апостол Петр заинтересовался и попросил показать ту самую записку. Развернул ее и…: «Да за такие слова тебе в раю не место!»

Попал мужик в ад. Встречает его чёрт и спрашивает, что произошло и как он к нему попал. Мужик и начал заново свою историю о том, как встретил старуху, которая дала ему записку и просила самому не читать, а дать другим прочитать. Жена из дома выгнала, лучший друг отрекся, милиционер под суд отдал, судья приговорил к казни, из рая тоже выгнали. Ну чёрту стало интересно и он попросил показать ему записку. Прочтя, черт заявил: «Да за такие слова тебе и в аду не место!».

Попал мужик в Лету. Очутился в лодке с каким-то старцем. Старец спросил, что же такого произошло, что мужик оказался в Лете? Мужик и рассказал, как встретил старуху, которая дала ему записку и сказала: «Сам не читай- дай другим почитать!» Жена из-за прочтенного выгнала из дома, лучший друг тоэже выставил за порог, милиционер отдал под суд, судья приговорил к расстрелу, апостол Петр выгнал из рая, а чёрт из ада. Старец заинтересовался и попросил дать ему записку. Прочитав слова в ней, старец воскликнул: «Да за такие слова тебе и в моей лодке не место!» …и вышвырнул мужика за борт.

Плывет мужик. Долго плывет. И думает: «Дай-ка я прочту, что же там написано!». Достает он эту записку, разворачивает, а буквы водой смыло.*)

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

