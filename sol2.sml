(*1*)
datatype expr = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr

datatype formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr

fun ops e =
	case e of
		NUM n => n
        | PLUS (e1,e2) => ops(e1) + ops(e2)
        | MINUS (e1,e2) => ops(e1) - ops(e2)

fun eval f =
	case f of
		TRUE => true
		| FALSE => false
		| NOT f => not(eval(f))
		| ANDALSO(f1, f2) => (eval(f1)) andalso (eval(f2))
		| ORELSE(f1, f2) => (eval(f1)) orelse (eval(f2))
		| IMPLY(f1, f2) => (not(eval(f1))) orelse (eval(f2))
		| LESS(e1, e2) => ops(e1)<ops(e2)

(*2*)
type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

fun checkStation(area:name list, e:metro) =
    let
        fun checkArea(area:name list, station:name) =
            if null area then false
            else hd area = station orelse checkArea(tl area, station)
    in
      case e of
        STATION e1 => checkArea(area, e1)
      | AREA(name1,e1) => checkStation(name1::area, e1)
      | CONNECT(e1,e2) => checkStation(area, e1) andalso checkStation(area, e2)
    end

fun checkMetro(m:metro) =
    case m of
        STATION m => false
      | AREA(name,m) => checkStation(name::[], m)
      | CONNECT(m1,m2) => checkMetro(m1) andalso checkMetro(m2)
        
(*3.i*)
datatype 'a lazylist = nullList
                    | cons of 'a * (unit -> 'a lazylist)

fun seq(first, last) = 
	if first = last
	then cons(last, fn()=>nullList)
	else cons(first, fn()=>seq(first+1, last))

fun infSeq(first) = 
    cons (first, fn()=>infSeq(first+1))

fun firstN(lazyListVal, n) =
	if n = 0
	then []
	else 
	case lazyListVal of
		nullList=>[]
		|cons(a, b)=>a::firstN(b(), n-1)

fun Nth(lazyListVal, n) = 
    if n=0 then NONE
    else 
    case lazyListVal of 
        nullList=>NONE
        | cons(a, b)=>if n=1 
                    then SOME a 
                    else Nth(b(),n-1)

fun filterMultiples(lazyListVal, n) = 
    case lazyListVal of 
        nullList=>nullList
        | cons(a, b)=>if a mod n = 0 
                    then filterMultiples(b(), n) 
                    else cons(a,fn()=>filterMultiples(b(),n))

(*3.ii*)
fun primes() = 
    let
        fun sieve(lazyListVal) = 
            case lazyListVal of nullList => nullList
                                | cons(a,b)=>cons(a, fn()=>sieve(filterMultiples(b(),a)))
    in
        sieve(infSeq(2))
    end