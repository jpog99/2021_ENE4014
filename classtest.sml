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
        

    in  
            (case e of 
              Constant i => if f i then count + 1 else count
            | Negate i1 =>  if (true_of_some_constants f i1) then count + 1 else count
            | Add (a,b) =>  if (true_of_some_constants f a  orelse true_of_some_constants f b) then count + 1 else count
            | Multiply (c,d) => if (true_of_some_constants f c  orelse true_of_some_constants f d) then count + 1 else count )
         
    end