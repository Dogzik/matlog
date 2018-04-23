open Grammar
open Utils

let repAB a b s = 
	let a_s = (string_of_expression a) in
	let b_s = (string_of_expression b) in
	Str.global_replace (Str.regexp ":") b_s (Str.global_replace (Str.regexp ";") a_s (Str.global_replace (Str.regexp "B") ":" (Str.global_replace (Str.regexp "A") ";" s)))
;;
	

let t_t_or e1 e2 = 
	let s = 
"(A->(A|B))
A
(A|B)" in
	repAB e1 e2 s
;;

let t_f_or e1 e2 = match e2 with 
	| Not(e) -> t_t_or e1 e
	| _		 -> ""
;;

let f_t_or e1 e2 =
	let s = 
"(B->(A|B))
B
(A|B)" in
	match e1 with
	| Not(e) -> repAB e e2 s
	| _		 -> ""
;;

let f_f_nor e1 e2 = 
	let s =
"(!A)
(!B)
(((!A)->((!B)->(!A)))->(A->((!A)->((!B)->(!A)))))
((!A)->((!B)->(!A)))
(A->((!A)->((!B)->(!A))))
((!A)->(A->(!A)))
(!A)
(A->(!A))
((A->(!A))->((A->((!A)->((!B)->(!A))))->(A->((!B)->(!A)))))
((A->((!A)->((!B)->(!A))))->(A->((!B)->(!A))))
(A->((!B)->(!A)))
((A->((!B)->A))->(A->(A->((!B)->A))))
(A->((!B)->A))
(A->(A->((!B)->A)))
(A->((A->A)->A))
(A->(A->A))
((A->(A->A))->((A->((A->A)->A))->(A->A)))
((A->((A->A)->A))->(A->A))
(A->A)
((A->A)->((A->(A->((!B)->A)))->(A->((!B)->A))))
((A->(A->((!B)->A)))->(A->((!B)->A)))
(A->((!B)->A))
((((!B)->A)->(((!B)->(!A))->(!(!B))))->(A->(((!B)->A)->(((!B)->(!A))->(!(!B))))))
(((!B)->A)->(((!B)->(!A))->(!(!B))))
(A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))
((A->((!B)->A))->((A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))->(A->(((!B)->(!A))->(!(!B))))))
((A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))->(A->(((!B)->(!A))->(!(!B)))))
(A->(((!B)->(!A))->(!(!B))))
((A->((!B)->(!A)))->((A->(((!B)->(!A))->(!(!B))))->(A->(!(!B)))))
((A->(((!B)->(!A))->(!(!B))))->(A->(!(!B))))
(A->(!(!B)))
(((!(!B))->B)->(A->((!(!B))->B)))
((!(!B))->B)
(A->((!(!B))->B))
((A->(!(!B)))->((A->((!(!B))->B))->(A->B)))
((A->((!(!B))->B))->(A->B))
(A->B)
(B->((B->B)->B))
(B->(B->B))
((B->(B->B))->((B->((B->B)->B))->(B->B)))
((B->((B->B)->B))->(B->B))
(B->B)
((A->B)->((B->B)->((A|B)->B)))
((B->B)->((A|B)->B))
((A|B)->B)
((!B)->((A|B)->(!B)))
(!B)
((A|B)->(!B))
(((A|B)->B)->(((A|B)->(!B))->(!(A|B))))
(((A|B)->(!B))->(!(A|B)))
(!(A|B))" in
	match (e1, e2) with
	| (Not(ee1), Not(ee2)) -> repAB ee1 ee2 s
	| _					   -> ""
;;


let t_t_and e1 e2 = 
	let s = 
"A
B
A->B->(A&B)
B->(A&B)
(A&B)" in
	repAB e1 e2 s
;;


let t_f_nand e1 e2 = 
	let s = 
"A
(!B)
(A&B)->B
(!B)->(A&B)->(!B)
((A&B)->(!B))
((A&B)->B)->((A&B)->(!B))->(!(A&B))
((A&B)->(!B))->(!(A&B))
(!(A&B))" in
	match e2 with
	| Not(e) -> repAB e1 e s
	| _		 -> ""
;;

let f_t_nand e1 e2 = 
	let s = 
"(!A)
B
(A&B)->A
(!A)->((A&B)->(!A))
(A&B)->(!A)
((A&B)->A)->((A&B)->(!A))->(!(A&B))
((A&B)->(!A))->(!(A&B))
(!(A&B))" in
	match e1 with 
	| Not(e) -> repAB e e2 s
	| _		 -> ""
;;

let f_f_nand e1 e2 = 
	let s = 
"(!A)
(!B)
(A&B)->A
(!A)->((A&B)->(!A))
(A&B)->(!A)
((A&B)->A)->((A&B)->(!A))->(!(A&B))
((A&B)->(!A))->(!(A&B))
(!(A&B))" in
	match (e1, e2) with
	| (Not(ee1), Not(ee2)) -> repAB ee1 ee2 s
	| _					   -> ""
;;

let t_t_impl e1 e2 =
	let s = 
"B->(A->B)
B
A->B" in
	repAB e1 e2 s
;;

let t_f_nimpl e1 e2 =
	let s =
"A
(!B)
((A->B)->(((A->B)->(A->B))->(A->B)))
((A->B)->((A->B)->(A->B)))
(((A->B)->((A->B)->(A->B)))->(((A->B)->(((A->B)->(A->B))->(A->B)))->((A->B)->(A->B))))
(((A->B)->(((A->B)->(A->B))->(A->B)))->((A->B)->(A->B)))
((A->B)->(A->B))
(A->((A->B)->A))
A
((A->B)->A)
(((A->B)->A)->(((A->B)->(A->B))->((A->B)->B)))
(((A->B)->(A->B))->((A->B)->B))
((A->B)->B)
((((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B)))))->(((A->B)->B)->(((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B)))))))
(((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B)))))
(((A->B)->B)->(((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B))))))
(((!B)->((A->B)->(!B)))->(((A->B)->B)->((!B)->((A->B)->(!B)))))
((!B)->((A->B)->(!B)))
(((A->B)->B)->((!B)->((A->B)->(!B))))
((((A->B)->B)->((!B)->((A->B)->(!B))))->((((A->B)->B)->(((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B))))))->(((A->B)->B)->((!B)->((!B)->((A->B)->(!B)))))))
((((A->B)->B)->(((!B)->((A->B)->(!B)))->((!B)->((!B)->((A->B)->(!B))))))->(((A->B)->B)->((!B)->((!B)->((A->B)->(!B))))))
(((A->B)->B)->((!B)->((!B)->((A->B)->(!B)))))
(((!B)->(((!B)->(!B))->(!B)))->(((A->B)->B)->((!B)->(((!B)->(!B))->(!B)))))
((!B)->(((!B)->(!B))->(!B)))
(((A->B)->B)->((!B)->(((!B)->(!B))->(!B))))
(((!B)->((!B)->(!B)))->(((A->B)->B)->((!B)->((!B)->(!B)))))
((!B)->((!B)->(!B)))
(((A->B)->B)->((!B)->((!B)->(!B))))
((((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))->(((A->B)->B)->(((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))))
(((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))
(((A->B)->B)->(((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B)))))
((((A->B)->B)->((!B)->((!B)->(!B))))->((((A->B)->B)->(((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B)))))->(((A->B)->B)->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))))
((((A->B)->B)->(((!B)->((!B)->(!B)))->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B)))))->(((A->B)->B)->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B)))))
(((A->B)->B)->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))
((((A->B)->B)->((!B)->(((!B)->(!B))->(!B))))->((((A->B)->B)->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))->(((A->B)->B)->((!B)->(!B)))))
((((A->B)->B)->(((!B)->(((!B)->(!B))->(!B)))->((!B)->(!B))))->(((A->B)->B)->((!B)->(!B))))
(((A->B)->B)->((!B)->(!B)))
((((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B)))))->(((A->B)->B)->(((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B)))))))
(((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B)))))
(((A->B)->B)->(((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B))))))
((((A->B)->B)->((!B)->(!B)))->((((A->B)->B)->(((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B))))))->(((A->B)->B)->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B)))))))
((((A->B)->B)->(((!B)->(!B))->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B))))))->(((A->B)->B)->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B))))))
(((A->B)->B)->(((!B)->((!B)->((A->B)->(!B))))->((!B)->((A->B)->(!B)))))
(((!B)->((A->B)->(!B)))->(((A->B)->B)->((!B)->((A->B)->(!B)))))
((!B)->((A->B)->(!B)))
(((A->B)->B)->((!B)->((A->B)->(!B))))
((((A->B)->B)->((!B)->((A->B)->B)))->(((A->B)->B)->(((A->B)->B)->((!B)->((A->B)->B)))))
(((A->B)->B)->((!B)->((A->B)->B)))
(((A->B)->B)->(((A->B)->B)->((!B)->((A->B)->B))))
(((A->B)->B)->((((A->B)->B)->((A->B)->B))->((A->B)->B)))
(((A->B)->B)->(((A->B)->B)->((A->B)->B)))
((((A->B)->B)->(((A->B)->B)->((A->B)->B)))->((((A->B)->B)->((((A->B)->B)->((A->B)->B))->((A->B)->B)))->(((A->B)->B)->((A->B)->B))))
((((A->B)->B)->((((A->B)->B)->((A->B)->B))->((A->B)->B)))->(((A->B)->B)->((A->B)->B)))
(((A->B)->B)->((A->B)->B))
((((A->B)->B)->((A->B)->B))->((((A->B)->B)->(((A->B)->B)->((!B)->((A->B)->B))))->(((A->B)->B)->((!B)->((A->B)->B)))))
((((A->B)->B)->(((A->B)->B)->((!B)->((A->B)->B))))->(((A->B)->B)->((!B)->((A->B)->B))))
(((A->B)->B)->((!B)->((A->B)->B)))
(((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))->(((A->B)->B)->((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))))
((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))
(((A->B)->B)->((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))))
((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->(((A->B)->B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))
(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))
(((A->B)->B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))
((((A->B)->B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((((A->B)->B)->((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))))->(((A->B)->B)->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))))
((((A->B)->B)->((((A->B)->B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))))->(((A->B)->B)->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))))
(((A->B)->B)->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))
((((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))->(((A->B)->B)->(((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))))
(((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))
(((A->B)->B)->(((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B)))))))
((((A->B)->B)->((!B)->((A->B)->B)))->((((A->B)->B)->(((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B)))))))->(((A->B)->B)->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))))
((((A->B)->B)->(((!B)->((A->B)->B))->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B)))))))->(((A->B)->B)->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B)))))))
(((A->B)->B)->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))
((((A->B)->B)->((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B))))))->((((A->B)->B)->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))->(((A->B)->B)->((!B)->(((A->B)->(!B))->(!(A->B)))))))
((((A->B)->B)->(((!B)->(((A->B)->B)->(((A->B)->(!B))->(!(A->B)))))->((!B)->(((A->B)->(!B))->(!(A->B))))))->(((A->B)->B)->((!B)->(((A->B)->(!B))->(!(A->B))))))
(((A->B)->B)->((!B)->(((A->B)->(!B))->(!(A->B)))))
((((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))->(((A->B)->B)->(((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))))
(((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))
(((A->B)->B)->(((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B))))))
((((A->B)->B)->((!B)->((A->B)->(!B))))->((((A->B)->B)->(((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B))))))->(((A->B)->B)->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))))
((((A->B)->B)->(((!B)->((A->B)->(!B)))->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B))))))->(((A->B)->B)->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B))))))
(((A->B)->B)->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))
((((A->B)->B)->((!B)->(((A->B)->(!B))->(!(A->B)))))->((((A->B)->B)->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))->(((A->B)->B)->((!B)->(!(A->B))))))
((((A->B)->B)->(((!B)->(((A->B)->(!B))->(!(A->B))))->((!B)->(!(A->B)))))->(((A->B)->B)->((!B)->(!(A->B)))))
(((A->B)->B)->((!B)->(!(A->B))))
((!B)->(!(A->B)))
(!B)
(!(A->B))" in
	match e2 with
	| Not(e) -> repAB e1 e s
	| _		 -> ""
;;

let f_t_impl e1 e2 = 
	let s = 
"B->(A->B)
B
(A->B)" in
	match e1 with
	| Not(e) -> repAB e e2 s
	| _		 -> ""
;;


let f_f_impl e1 e2 =
	let s = 
"(!A)
(!B)
((A->((!B)->A))->(A->(A->((!B)->A))))
(A->((!B)->A))
(A->(A->((!B)->A)))
(A->((A->A)->A))
(A->(A->A))
((A->(A->A))->((A->((A->A)->A))->(A->A)))
((A->((A->A)->A))->(A->A))
(A->A)
((A->A)->((A->(A->((!B)->A)))->(A->((!B)->A))))
((A->(A->((!B)->A)))->(A->((!B)->A)))
(A->((!B)->A))
(((!A)->((!B)->(!A)))->(A->((!A)->((!B)->(!A)))))
((!A)->((!B)->(!A)))
(A->((!A)->((!B)->(!A))))
((!A)->(A->(!A)))
(!A)
(A->(!A))
((A->(!A))->((A->((!A)->((!B)->(!A))))->(A->((!B)->(!A)))))
((A->((!A)->((!B)->(!A))))->(A->((!B)->(!A))))
(A->((!B)->(!A)))
((((!B)->A)->(((!B)->(!A))->(!(!B))))->(A->(((!B)->A)->(((!B)->(!A))->(!(!B))))))
(((!B)->A)->(((!B)->(!A))->(!(!B))))
(A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))
((A->((!B)->A))->((A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))->(A->(((!B)->(!A))->(!(!B))))))
((A->(((!B)->A)->(((!B)->(!A))->(!(!B)))))->(A->(((!B)->(!A))->(!(!B)))))
(A->(((!B)->(!A))->(!(!B))))
((A->((!B)->(!A)))->((A->(((!B)->(!A))->(!(!B))))->(A->(!(!B)))))
((A->(((!B)->(!A))->(!(!B))))->(A->(!(!B))))
(A->(!(!B)))
(((!(!B))->B)->(A->((!(!B))->B)))
((!(!B))->B)
(A->((!(!B))->B))
((A->(!(!B)))->((A->((!(!B))->B))->(A->B)))
((A->((!(!B))->B))->(A->B))
(A->B)" in
	match (e1, e2) with
	| (Not(ee1), Not(ee2)) -> repAB ee1 ee2 s
	| _					   -> ""
;;

let add_double_neg expr =
	let s =
"A
((!A)->(((!A)->(!A))->(!A)))
((!A)->((!A)->(!A)))
(((!A)->((!A)->(!A)))->(((!A)->(((!A)->(!A))->(!A)))->((!A)->(!A))))
(((!A)->(((!A)->(!A))->(!A)))->((!A)->(!A)))
((!A)->(!A))
(A->((!A)->A))
((!A)->A)
(((!A)->A)->(((!A)->(!A))->(!(!A))))
(((!A)->(!A))->(!(!A)))
(!(!A))" in
	Str.global_replace (Str.regexp "A") (string_of_expression expr) s
;;

let contrpos a b = 
	let s =
"(((A)->(B))->((!(B))->((A)->(B))))->(((A)->(B))->(((A)->(B))->((!(B))->((A)->(B)))))
(((A)->(B))->((!(B))->((A)->(B))))
((A)->(B))->(((A)->(B))->((!(B))->((A)->(B))))
((A)->(B))->(((A)->(B))->((A)->(B)))
((A)->(B))->((((A)->(B))->((A)->(B)))->((A)->(B)))
(((A)->(B))->(((A)->(B))->((A)->(B))))->((((A)->(B))->(((A)->(B))->((A)->(B)))->((A)->(B)))->(((A)->(B))->((A)->(B))))
(((A)->(B))->(((A)->(B))->((A)->(B)))->((A)->(B)))->(((A)->(B))->((A)->(B)))
((A)->(B))->((A)->(B))
(((A)->(B))->((A)->(B)))->((((A)->(B))->(((A)->(B))->((!(B))->((A)->(B)))))->(((A)->(B))->((!(B))->((A)->(B)))))
(((A)->(B))->(((A)->(B))->((!(B))->((A)->(B)))))->(((A)->(B))->((!(B))->((A)->(B))))
((A)->(B))->((!(B))->((A)->(B)))
((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))))->(((A)->(B))->((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))))
((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))))
((A)->(B))->((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))))
(((A)->(B))->(((A)->(!(B)))->(!(A))))->(((A)->(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))
(((A)->(B))->(((A)->(!(B)))->(!(A))))
((A)->(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))
(((A)->(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((((A)->(B))->((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))))->(((A)->(B))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))))
(((A)->(B))->((((A)->(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))))->(((A)->(B))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))))
((A)->(B))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))
(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))->(((A)->(B))->(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A)))))))
(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))
((A)->(B))->(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))
(((A)->(B))->((!(B))->((A)->(B))))->((((A)->(B))->(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A)))))))->(((A)->(B))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A)))))))
(((A)->(B))->(((!(B))->((A)->(B)))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A)))))))->(((A)->(B))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))
((A)->(B))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A)))))
(((A)->(B))->((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A))))))->((((A)->(B))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))->(((A)->(B))->((!(B))->(((A)->(!(B)))->(!(A))))))
(((A)->(B))->(((!(B))->(((A)->(B))->(((A)->(!(B)))->(!(A)))))->((!(B))->(((A)->(!(B)))->(!(A))))))->(((A)->(B))->((!(B))->(((A)->(!(B)))->(!(A)))))
((A)->(B))->((!(B))->(((A)->(!(B)))->(!(A))))
((!(B))->((!(B))->(!(B))))->(((A)->(B))->((!(B))->((!(B))->(!(B)))))
((!(B))->((!(B))->(!(B))))
((A)->(B))->((!(B))->((!(B))->(!(B))))
((!(B))->(((!(B))->(!(B)))->(!(B))))->(((A)->(B))->((!(B))->(((!(B))->(!(B)))->(!(B)))))
((!(B))->(((!(B))->(!(B)))->(!(B))))
((A)->(B))->((!(B))->(((!(B))->(!(B)))->(!(B))))
(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))->(((A)->(B))->(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B))))))
(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))
((A)->(B))->(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))
(((A)->(B))->((!(B))->((!(B))->(!(B)))))->((((A)->(B))->(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B))))))->(((A)->(B))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B))))))
(((A)->(B))->(((!(B))->((!(B))->(!(B))))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B))))))->(((A)->(B))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))
((A)->(B))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B))))
(((A)->(B))->((!(B))->(((!(B))->(!(B)))->(!(B)))))->((((A)->(B))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))->(((A)->(B))->((!(B))->(!(B)))))
(((A)->(B))->(((!(B))->(((!(B))->(!(B)))->(!(B))))->((!(B))->(!(B)))))->(((A)->(B))->((!(B))->(!(B))))
((A)->(B))->((!(B))->(!(B)))
(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B))))))->(((A)->(B))->(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B)))))))
(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B))))))
((A)->(B))->(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B))))))
((!(B))->((A)->(!(B))))->(((A)->(B))->((!(B))->((A)->(!(B)))))
((!(B))->((A)->(!(B))))
((A)->(B))->((!(B))->((A)->(!(B))))
(((A)->(B))->((!(B))->((A)->(!(B)))))->((((A)->(B))->(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B)))))))->(((A)->(B))->((!(B))->((!(B))->((A)->(!(B)))))))
(((A)->(B))->(((!(B))->((A)->(!(B))))->((!(B))->((!(B))->((A)->(!(B)))))))->(((A)->(B))->((!(B))->((!(B))->((A)->(!(B))))))
((A)->(B))->((!(B))->((!(B))->((A)->(!(B)))))
(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B))))))->(((A)->(B))->(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B)))))))
(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B))))))
((A)->(B))->(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B))))))
(((A)->(B))->((!(B))->(!(B))))->((((A)->(B))->(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B)))))))->(((A)->(B))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B)))))))
(((A)->(B))->(((!(B))->(!(B)))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B)))))))->(((A)->(B))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B))))))
((A)->(B))->(((!(B))->((!(B))->((A)->(!(B)))))->((!(B))->((A)->(!(B)))))
((!(B))->((A)->(!(B))))->(((A)->(B))->((!(B))->((A)->(!(B)))))
((!(B))->((A)->(!(B))))
((A)->(B))->((!(B))->((A)->(!(B))))
(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))->(((A)->(B))->(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A))))))
(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))
((A)->(B))->(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))
(((A)->(B))->((!(B))->((A)->(!(B)))))->((((A)->(B))->(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A))))))->(((A)->(B))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A))))))
(((A)->(B))->(((!(B))->((A)->(!(B))))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A))))))->(((A)->(B))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))
((A)->(B))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A))))
(((A)->(B))->((!(B))->(((A)->(!(B)))->(!(A)))))->((((A)->(B))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))->(((A)->(B))->((!(B))->(!(A)))))
(((A)->(B))->(((!(B))->(((A)->(!(B)))->(!(A))))->((!(B))->(!(A)))))->(((A)->(B))->((!(B))->(!(A))))
((A)->(B))->((!(B))->(!(A)))" in
	repAB a b s
;;

let a_or_not_a expr =
	let a = Var("A") in
	let s = "A->(A|!A)\n" ^
	(contrpos a (Bin(Or, a, Not(a)))) ^
	"\n!(A|!A)->!A\n!A->(A|!A)\n" ^
	(contrpos (Not(a)) (Bin(Or, a, Not(a)))) ^
	"\n!(A|!A)->!!A\n" ^
	"(!(A|!A)->!A)->(!(A|!A)->!!A)->!!(A|!A)\n" ^
	"(!(A|!A)->!!A)->!!(A|!A)\n!!(A|!A)\n" ^
	"!!(A|!A)->(A|!A)\n(A|!A)" in
	Str.global_replace (Str.regexp "A") (string_of_expression expr) s
;;

let exclude a na f = 
	let a_s = (string_of_expression a) in
	let na_s = "!(" ^ a_s ^ ")" in
	let f_s = (string_of_expression f) in
	let tmp = (a_or_not_a a) ^ "\n" ^
	("(" ^ a_s ^ "->" ^ f_s ^ ")->(" ^ na_s ^ "->" ^ f_s ^ ")->((" ^ a_s ^ "|" ^ na_s ^ ")->" ^ f_s ^ ")\n") ^
	("(" ^ na_s ^ "->" ^ f_s ^ ")->((" ^ a_s ^ "|" ^ na_s ^ ")->" ^ f_s ^ ")\n") ^ 
	("(" ^ a_s ^ "|" ^ na_s ^ ")->" ^ f_s ^ "\n") ^ f_s in
	split tmp "\n"
;;
	



let get_bin_proof a b expr = match expr with
	| Bin(Or, a1, b1) when (a = a1 && b = b1) -> t_t_or a b
	| Bin(Or, a1, b1) when (b = a1 && a = b1) -> t_t_or b a
	| Bin(Or, a1, b1) when (a = a1 && b = Not(b1)) -> t_f_or a b
	| Bin(Or, a1, b1) when (b = a1 && a = Not(b1)) -> t_f_or b  a
	| Bin(Or, a1, b1) when (a = Not(a1) && b = b1) -> f_t_or a b
	| Bin(Or, a1, b1) when (b = Not(a1) && a = b1) -> f_t_or b a
	| Not(Bin(Or, a1, b1)) when (a = Not(a1) && b = Not(b1)) -> f_f_nor a b
	| Not(Bin(Or, a1, b1)) when (b = Not(a1) && a = Not(b1)) -> f_f_nor b a

	| Bin(And, a1, b1) when (a = a1 && b = b1) -> t_t_and a b
	| Bin(And, a1, b1) when (b = a1 && a = b1) -> t_t_and b a
	| Not(Bin(And, a1, b1)) when (a = a1 && b = Not(b1)) -> t_f_nand a b
	| Not(Bin(And, a1, b1)) when (b = a1 && a = Not(b1)) -> t_f_nand b a
	| Not(Bin(And, a1, b1)) when (a = Not(a1) && b = b1) -> f_t_nand a b
	| Not(Bin(And, a1, b1)) when (b = Not(a1) && a = b1) -> f_t_nand b a
	| Not(Bin(And, a1, b1)) when (a = Not(a1) && b = Not(b1)) -> f_f_nand a b
	| Not(Bin(And, a1, b1)) when (b = Not(a1) && a = Not(b1)) -> f_f_nand b a

	| Bin(Impl, a1, b1) when (a = a1 && b = b1) -> t_t_impl a b
	| Bin(Impl, a1, b1) when (b = a1 && a = b1) -> t_t_impl b a
	| Not(Bin(Impl, a1, b1)) when (a = a1 && b = Not(b1)) -> t_f_nimpl a b
	| Not(Bin(Impl, a1, b1)) when (b = a1 && a = Not(b1)) -> t_f_nimpl b a
	| Bin(Impl, a1, b1) when (a = Not(a1) && b = b1) -> f_t_impl a b
	| Bin(Impl, a1, b1) when (b = Not(a1) && a = b1) -> f_t_impl b a
	| Bin(Impl, a1, b1) when (a = Not(a1) && b = Not(b1)) -> f_f_impl a b
	| Bin(Impl, a1, b1) when (b = Not(a1) && a = Not(b1)) -> f_f_impl b a
	| _ -> "The cake is a lie!"
;;