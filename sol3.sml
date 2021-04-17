datatype pattern = Wildcard | Variable of string | UnitP
                | ConstP of int | TupleP of pattern list
                | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
             | Constructor of string * valu

(*1*)
fun check_pat p = 
    let
        fun make_list ls = 
            case ls of 
                Variable s => [s]
                | ConstructorP(s, ps) => make_list ps
                |TupleP t1 => foldl(fn (p,acc) => acc @ make_list p) [] t1
                |_ => []
        fun check l = 
	        case l of
		        [] => true
		        |x::xs => (not(List.exists(fn a => a = x) xs)) andalso check(xs)

    val p1 = make_list p

    in 
     		check p1
	end

(*2*)
fun match (v, p) = 
    case p of 
        Wildcard => SOME[]

        | Variable s => SOME[(s,v)]

        | UnitP => (case v of
                    Unit => SOME []
                    |_=> NONE)

        |ConstP i => (case v of
                    Const i1 => if i = i1 then SOME [] else NONE
                    |_ => NONE)

		|ConstructorP(str, pat) =>
			(case v of
				Constructor(str1, vl) => if str = str1 then match(vl, pat) else NONE
				|_ => NONE)

         |TupleP ps => (case v of
                    Tuple v1 => if (List.length v1) = (List.length ps)
                                andalso let    
                                        fun check_match x = 
                                            case x of 
                                                [] => true
                                                |x::xs => if(match x = NONE) 
                                                then false
                                                else check_match xs 
                                        in 
                                            check_match(ListPair.zip(v1,ps) )
                                        end
                                then 
                                let fun add_match x = 
                                    case x of
                                        [] => []
                                        |x::xs =>if(match x = NONE)
                                                 then add_match xs
                                                 else valOf(match x) @ add_match xs
                                in
                                    SOME(add_match (ListPair.zip(v1,ps)))
                                end
                                else NONE )

(*3*)
type name = string
datatype RSP =
            ROCK
            | SCISSORS
            | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament =
            PLAYER of name * (RSP strategy ref)
            | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let 
        val Cons(rsp, func) = !strategyRef 
    in
        strategyRef := func();
        rsp
    end

fun whosWinner (t) = 
    let 
        fun game (PLAYER(name1,strat1), PLAYER(name2,strat2)) = 
            let
                val res = (next strat1 , next strat2)
            in
                case res of
                    (ROCK,PAPER) => PLAYER(name1,strat1)
                    |(ROCK,SCISSORS) => PLAYER(name2,strat2)
                    |(PAPER,ROCK) => PLAYER(name1,strat1)
                    |(PAPER,SCISSORS) => PLAYER(name2,strat2)
                    |(SCISSORS,ROCK) => PLAYER(name1,strat1)
                    |(SCISSORS,PAPER) => PLAYER(name2,strat2)
                    |_ => game(PLAYER(name1,strat1), PLAYER(name2,strat2))  (*in case of tie*)
            end
    in
        case t of
            PLAYER(name,strat) => PLAYER(name,strat)
            | MATCH (p1,p2) => game(whosWinner p1 , whosWinner p2)
    end
