(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem 1a *)
fun all_except_option(x, y) =
    let fun aux(x, head, tail) =
	    case tail of
		[] => NONE
	      | x' :: xs => if same_string(x', x)
			    then SOME (head @ xs)
			    else aux(x, head @ [x'], xs)
    in
	aux(x, [], y)
    end
    
(* Problem 1b *)
fun get_substitutions1(x, y) =
    let fun aux r =
	    case r of
		NONE => []
	      | SOME i => i 
    in
	case x of
	    [] => []
	  | x'::xs => aux(all_except_option(y, x')) @ get_substitutions1(xs, y)
    end

(* Problem 1c *)
fun get_substitutions2(x, y) =
    let fun aux r =
	    case r of
		NONE => []
	      | SOME i => i
			      
	fun recur_aux(rm_x, y, res) =
	    case rm_x of
		[] => res
	      | x'::xs => recur_aux(xs, y, res @ aux(all_except_option(y, x')))
    in
	recur_aux(x, y, [])
    end
									  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


