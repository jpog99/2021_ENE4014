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

fun checkMetro m = 
    case m of   
        
(*3*)