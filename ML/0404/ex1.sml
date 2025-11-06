fun identity(x) = x;

fun f(x) = if x < 10 then identity else rev;

val x = (1,2);
val y = (2,3);
x=y;

fun rev1 (L) = if L = nil then nil
else rev(tl(L)) @ [hd(L)];

fun rev2(nil) = nil
|rev2(x::xs) = rev2(xs) @ [x];

fun rev3 = if null(L) then nil 
else rev(tl(L)) @ [hd(L)];


(*approximate integrals*)
fun trap (a,b,n,F) = if n<=0 orelse b-a <=0.0 then 0.0
else
let val delta = (b-a)/real(n)
in
delta * (F(a) + F(a+delta))/2.0 + trap(a+delta,b,n-1,F)
end;

fun trap2(a,b,n,F) = if n<=0 orelse b-a <=0.0 then 0.0
else
let 
val delta = (b-a)/real(n);
fun trap1(x,0) = 0.0
|trap1(x,i) = delta * (F(x) + F(x + delta))/2.0 + trap1(x+delta, i - 1);
in trap1(a,n)
end;

(*basic built-in lists functions*)

(**each element is passed through F*)
fun simpleMap(F,nil) = nil
|simpleMap(F,x::xs) = F(x) :: simpleMap(F,xs);

(*reduce*)
(*puts an operation in the middle of all elements of the list*)
 fun reduce(F,nil) = raise EmptyList
 |reduce(F,[a]) = a
 |reduce(F,x::xs) = F(x,reduce(F,xs));

 (**compute variance*)
 fun square(x:real) = x*x;
 fun plus(x:real,y) = x+y;

 fun variance(L) = 
 len val n = real(List.lenght(L))
 in 
 reduce(plus,simpleMap(square,L)) / n - square(reduce(plus,L) / n);

(*filter*)
fun filter(P,nil) = nil
|filter(P,x::xs) = if P(x) then x::filter(P,xs) else filter(P,xs);


fun tabulate(x:real, delta:real, 0,F) = ()
| tabulate(x,delta,n,F) = (
    print(Real.toString(x));
    print("\t");
    print(Real.toString(F(x)));
    print("\n");
    tabulate(x+delta, delta, n - 1, F);
);





(**replace every negative with 0*)
fun fixNegative(x) = if x < 0.0 then 0.0 else x;
simpleMap(fixNegative, L)

simpleMap(fn(x) => x + 1, L)

(**list of strings cuts the one grater than five chars*)
simpleMap(fn (c) => if size(c) <= 5 then c else substring (c,0,5) , L);

(**find maximum of list of reals*)
funMaxReals = reduce (fn(x,y) => if x > y then x else y, L);

(**min*)
reduce(fn(x,y) => if x < y then x else y, L);

(**logical or*)
reduce(f(x,y) => x orelse y, L);

(***Greater than 0*)
filter(fn(x) =>  x > 0.0,L)

(**Between 1 and 2 (real)*)
filter(fn(x) => (x >= 1.0 andalso x <= 2.0), L);

(**keep strings up to 3 chars*)
filter(fn(s) => size(s) < 3, L );