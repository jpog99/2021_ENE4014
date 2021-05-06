datatype exp = Constant of int
| Negate of exp
| Add of exp * exp
| Multiply of exp * exp

fun true_of_all_constants(f,e) =
case e of
Constant i => f i
| Negate e1 => true_of_all_constants(f,e1)
| Add(e1,e2) => true_of_all_constants(f,e1)
andalso true_of_all_constants(f,e2)
| Multiply(e1,e2) => true_of_all_constants(f,e1)
andalso true_of_all_constants(f,e2)

fun all_even_exp e =
true_of_all_constants((fn x => x mod 2 = 0),e)
  
fun true_of_some_constants f e m = 
  let
    fun count_of_true_constants (f,e) =
      let
          val count = 0
      in  
              (case e of 
                Constant i => if f i then count+1 else count
              | Negate i1 =>  count_of_true_constants (f,i1)
              | Add (a,b) =>  count_of_true_constants (f,a)  + count_of_true_constants (f,b) 
              | Multiply (a,b) => count_of_true_constants (f,a)  + count_of_true_constants (f,b))
      end 
  in
    count_of_true_constants(f,e) >= m
  end
      



(*from chapter 2*)
fun apply_f f x y =
  f(x,y) 

(*from chapter 3 pg26*)
fun max_constant e = 
  case e of 
    Constant i => i
    | Negate i1 => max_constant(i1)
    | Add (a,b) => if max_constant(a) > max_constant(b) then max_constant(a) else max_constant(b)
    | Multiply (a,b) => if max_constant(a) > max_constant(b) then max_constant(a) else max_constant(b) 

(*from chaper 4 pg 11*)
fun nondecreasing xs = 
  case xs of  
    [] => true
    |[x] => true
    |x::y => x<=hd y andalso nondecreasing y

fun multsign (x1,x2) =
  let 
    fun sign s = if s = 0 then "Z" else if s<0 then "N" else "P"
  in
    case (sign(x1),sign(x2)) of
      ("P","P") => "P"
      |("N","N") => "P"
      |("Z",_) => "Z"
      |(_,"Z") => "Z"
      |_ => "N"
  end   

  (*from chaper 4 pg 15*) 
exception div0
fun divide (x,y) = 
  case (x,y) of
    (_,0) => raise div0
    |(0,_) => 0
    |_=> x div y

fun true_div x y =
  (divide (x,y)) handle div0 => ~1

  (*from chaper 4 pg 28*)

  fun sum xs = 
    let 
      fun aux(xs,acc) = 
         case xs of
          [] => acc
          |x::y => aux(y,x+acc)
    in
      aux(xs,0)
    end

 (*from chaper 4 pg 28*)
 fun times_until_0 (f,x) = 
  let
    fun aux (f, acc) = 
      if acc = 0 
      then 0
      else 1 + times_until_0(f, f x) 
  in
    aux(f, f x)
  end

(*practice from website*)

(*map func with tail rec*)
fun tailmap f xs = 
  let 
    fun aux f acc ls = 
      case ls of
        [] => acc
        |x::xs => aux f (f x::acc) xs

    val reverse = fn l => case l of []=>[] | a::b=> reverse b @ [a]
  in
    reverse(aux f [] xs)
  end

(*filter func with tail rec*)
fun tailfilter f xs = 
  let
    fun aux f acc ls = 
      case ls of
        [] => acc
        |x::xs => if f x
                  then aux f (x :: acc) xs
                  else aux f acc xs
    
    val reverse = fn l => case l of []=>[] | a::b=> reverse b @ [a]
  in
    reverse(aux f [] xs)
  end    

fun tail_countdown x = 
  let
    fun aux n xs =
      if n=0
      then xs
      else aux (n-1) (xs @ [n])
  in
    aux x []
  end          