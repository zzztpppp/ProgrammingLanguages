fun is_order(d1: int*int*int, d2: int*int*int) =
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
					  
