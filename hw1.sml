fun is_older(d1: int*int*int, d2: int*int*int) =
    if (#1 d1) > (#1 d2)
    then true
    else if (#2 d1) > (#2 d2)
    then true
    else if (#3 d1) > (#3 d2)
    then true
    else false

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(t1 dates, month)


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
    then [#2 (hd dates)] :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) :: dates_in_motns(dates, tl months)


fun get_nth(l_stirngs : string list, n : int) =
    if n = 1
    then hd l_strings
    else get_nth(tl l_strings, n -1)

fun date_to_string(date : int*int*int) =
    let val month = #2 date
	val year = #1 date
	val day = #3 date
	val day_year = Int.toString(day) ^ "," ^  " " ^ Int.toString(year)
	val month_strings = ["Januaury", "February", "March", "April", "May",
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
	number_before_reaching_sum(days_of_month, day)
    end

fun month_range(day1 : int, day2 : int) =
    let val month1 = what_month(day1)
	val month2 = what_month(day2 + 1)
	fun range(from : int, to : int) =
	    if from = to
	    then []
	    else [from] :: range(from - 1, to)
    in
	range(month1, month2)
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun get_oldest(date : int*int*int, dates : (int*int*int) list) =
		if is_older(date, hd dates)
		then is_older(date, tl dates)
		else if  not is_older(date, hd dates)
		then is_older(hd dates, tl dates)
		else if null dates
		then SOME(date)
    end
	    
					  
