(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Problem 1*)
fun only_capitals(sl) =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) sl
		
(* Problem 2*)
fun longest_string1(strl) =
    case strl of
	[] => ""
      | strl' => List.foldl (fn (x, y) => if (String.size x) > (String.size y) then x else y) "" strl'  

(* Problem 3 *)
fun longest_string2(strl) =
    case strl of
	[] => ""
      | strl' => List.foldl (fn (x, y) => if (String.size x) >= (String.size y) then x else y) "" strl'

(* Problem 4 *)
fun longest_string_helper f s =
    case s of
	[] => ""
      | s' => List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" s' 

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* Problem 5 *)
val longest_capitalized  = longest_string1 o only_capitals		      
		       
(* Problem 6 *)
val rev_string =  String.implode o rev o String.explode

(* Problem 7 *)
fun first_answer f x =
    case x of
	[] => raise NoAnswer
      | x'::xs' => case (f x') of
		       SOME(y) => y
		     | NONE  => first_answer f xs' 
					     
(* Problem 8 *)
fun all_answers f x =
    let fun aux(acc, f, x) =
	    List.foldl (fn (x', y) => case (f x') of 
					NONE => y
				       | SOME y' => y'@y 
		       ) acc x
    in
	case (x, aux([], f, x)) of
	    ([], _) => SOME []
	  | (_,[]) => NONE
	  | (_, x') => SOME x'
    end



