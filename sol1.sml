(*1*)
fun merge(xs : int list , ys : int list) = 
    if null xs
        then ys
    else if null ys
        then xs
    else if (hd xs) < (hd ys)
        then hd xs :: merge(tl xs , ys)
    else hd ys :: merge(xs , tl ys)

(*2*)
fun reverse(xs : int list) =
    if null xs then xs
    else
        let 
            fun append(arr1:int list , arr2:int list) = 
                if null (tl arr1) 
                then hd arr1 :: arr2
                else append(tl arr1 , hd arr1::arr2)
        in 
            append(xs,[])
        end

(*3*)
fun pi(a:int , b:int , f:int->int) = 
    if a>b then 1
    else f(a)*pi(a+1,b,f)

(*4*)
fun digits(n:int) = 
    if n<10 then [n] 
    else 
    let val result = n mod 10 :: reverse(digits(n div 10))
    in reverse(result)
    end

(*5*)
fun additivePersistence(n:int) = 
    if n div 10 = 0 then 0
    else
    let 
        fun sum_list (xs: int list) = 
	    if null xs
	    then 0
	    else hd(xs)+sum_list(tl(xs));
    in 
        additivePersistence(sum_list(digits(n))) + 1
    end

fun digitalRoot(n:int) = 
    if n div 10 = 0 then n
    else
    let 
        fun sum_list (xs: int list) = 
	    if null xs
	    then 0
	    else hd(xs)+sum_list(tl(xs));
    in 
        digitalRoot(sum_list(digits(n)))
    end