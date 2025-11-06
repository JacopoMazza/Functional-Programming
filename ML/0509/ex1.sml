exception EmptyList;

(**left association reduce*)
fun lReduce(F,nil) = raise EmptyList
|lReduce(F,[x]) = x
|lReduce(F,x::y::zs) = lreduce(F,F(x,y)::zs); 


(**argument are not a tuple, but expnent 2 of 3.0 will give a function thta can then be applied to an int*)
exponent2 x 0 = 1.0
|exponent2 x y = x* exponent2 x (y - 1);

(**function takes a list of functions and applies it to a value*)
fun applyList nil  _ = nil
|applyList (F::Fs) a = F(a) :: (applyList Fs a);


(***takes a function and applies function to all the elements*)
fun makeFnList F nil = nil
|makeFnList F (x::xs) = F(x) :: (makeFnList F xs);


(**checks if one list is a prefix of the other*)
fun ss1(nil, _) = true
|ss1(_, nil) = false
|ss1(x::xs, y::ys) = (x=y andalso ss1(xs,ys));


fun ss2(x,nil) = ss1(x,nil)
|ss2(x,y::ys) = ss1(x,y::ys) orelse ss2(x,ys);

fun substring x y = ss2(explode(x), explode(y));


(***curry function*)
fun curry F x1 x2 x3 = F(x1,x2,x3);

fun uncurry F(x1,x2,x3) = F x1 x2 x3;

fun comp(F,G,x) = G(F(x));

fun F x = x + 3
fun G y = y*y + 2*y;
fun H = G o F;

(***COMPOSITION FUNCTIONS*)
fun comp F G = 
let fun C x = G(F(x))
in C
end;

(**map function*)
fun map F = 
let fun M nil = nil
|M(x::xs) = F x :: M xs
in M
end;


fun foldr F y nil = y
| foldr F y (x::xs)