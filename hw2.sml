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

(* Problem 1d *)
fun similar_names(names_list, name) =
    let val {first=f, middle=m, last=l} = name
	val similar_first_names = get_substitutions2(names_list, f)
	fun aux(similar_first_names, similar_names_list) =
	    case similar_first_names of
		[] => similar_names_list
	      | x'::xs => aux(xs, ({first=x', middle=m, last=l})::similar_names_list)
    in
	aux(similar_first_names, [name])
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
(* Problem 2a *)
fun card_color(card) =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

(* Problem 2b *)
fun card_value card =
    case card of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

(* Problem 2c *)
fun remove_card(cs, c, e) =
    let fun aux (head, cs, c, e) =
	    case cs of
	        [] => raise e
	      | x'::xs => if x' = c
			  then head @ xs
			  else aux(head @ [x'], xs,c,e)
    in
	aux([], cs, c, e)
    end
	
(* Problem of 2d *)
fun all_same_color cs =
    case cs of
	[] => true
      | x'::[] => true 
      | x'::(y'::xs) => if card_color(x') = card_color(y')
			then all_same_color(y'::xs)
			else false

(* Problem 2e *)
fun sum_cards(cs) =
    let fun aux(sum, cs) =
	    case cs of
		[] => sum
	      | x'::xs => aux(sum + card_value(x'), xs)
    in
	aux(0, cs)
    end

(* Problem 2f *)
fun score (cs, goal) =
    let val preliminary_score = goal - sum_cards(cs)
    in
	if all_same_color(cs)
	then preliminary_score div 2
	else preliminary_score
    end


	    
	     
	    
