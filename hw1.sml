fun is_older(d1: int*int*int, d2: int*int*int) =
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d1) > (#1 d2)
    then false
    else
	if (#2 d1) < (#2 d2)
	then true
	else if (#2 d1) > (#2 d2)
	then false
	else
	    if (#3 d1) < (#3 d2)
	    then true
	    else if (#3 d1) > (#3 d2)
	    then false
	    else false

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month((tl dates), month)


fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null dates orelse null months
    then 0
    else if null (tl months)
    then number_in_month(dates, hd months)
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then [(hd dates)] @  dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else (dates_in_month(dates, hd months)) @ (dates_in_months(dates, tl months))


fun get_nth(l_strings : string list, n : int) =
    if n = 1
    then hd l_strings
    else get_nth(tl l_strings, n -1)

fun date_to_string(date : int*int*int) =
    let val month = #2 date
	val year = #1 date
	val day = #3 date
	val day_year = Int.toString(day) ^ "," ^  " " ^ Int.toString(year)
	val month_strings = ["January", "February", "March", "April", "May",
			     "June", "July", "August", "September", "October",
			     "November", "December"]
    in
	get_nth(month_strings, month) ^ " " ^ day_year
    end

fun number_before_reaching_sum(sum : int, ns : int list) =
    let fun help(sum : int, ns : int list, sum_before : int) =
	    if sum_before + hd ns < sum
	    then 1 + help(sum, tl ns, sum_before + hd ns)
	    else 0
    in
	help(sum , ns, 0)
    end

fun what_month(day : int ) =
    let val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31]
    in
	number_before_reaching_sum(day, days_of_month) + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else let val old = hd dates
	     val old_left = oldest(tl dates)
	 in
	     if isSome(old_left)
	     then
		 if is_older(old, valOf(old_left))
		 then SOME(old)
		 else old_left
	     else
		 SOME(old)
	 end

fun is_in(num : int, l : int list) =
    (* Check if a given number is in l *)
    if null l
    then false
    else
	if (hd l) = num
	then true
	else is_in(num , tl l)

fun remove_duplicates(l) =
    (* Remove the duplicated value in l *)
    let fun sift(unique_list : int list, origin_list : int list) =
	    if null origin_list
	    then unique_list
	    else if not(is_in( hd origin_list, unique_list))
	    then sift((hd origin_list) :: unique_list, tl origin_list)
	    else
		sift(unique_list, tl origin_list)
    in
	sift([], l)
    end

fun number_in_months_challenge(dates : (int*int*int) list, month : int list) =
    let val unique_month = remove_duplicates month
    in
	number_in_months(dates, unique_month)
    end

fun reasonable_date(date : int*int*int) =
    let val year = #1 date
	val month = #2 date
	val days = #3 date
	val tmp1 = ((year mod 4) = 0)
	val tmp2 = ((year mod 100) <> 0)
	val tmp3 = ((year mod 400) = 0)
	val is_leap = tmp1 orelse (tmp2 andalso tmp3)
	val thirty1_days = [1,3,5,7,8,10,12]
	val thirty_days = [4,6,9,11]
		  
    in
	if (year > 0) andalso month >=1 andalso month <= 12 andalso days > 0 andalso days <=31
	then
	    if  is_in(month, thirty1_days)
	    then days <= 31
	    else if  is_in(month, thirty_days)
	    then days <= 30
	    else if is_leap andalso month = 2
	    then days <= 29
	    else days <= 28
	else false
    end		
   



				       
