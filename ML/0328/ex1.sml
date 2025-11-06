fun doubleExp(x:real, 0) = x
| doubleExp (x,i) = let
val y = doubleExp(x, i - 1)
in 
    y * y
end;


fun sumPairs(nil) = (0,0)
| sumPairs((x,y)::zs) = 
let
val (z1,z2) = sumPairs(zs)
in
(x+z1, y+z2)
end;


fun sumList(nil) = (0,0)
| sumList([x]) = (0,x)
| sumList(x::y::zs) = 
let
val (sumEven,sumOdd) = sumList(zs)
in 
(sumEven + y, sumOdd + x)
end;

fun printList (nil) = ()
|fun printList(x::xs) = (
    print(Int.toString(x)); print("\n"); printList(xs)
);

fun factorial 0 = 1
| factorial n = n*fact(n -1);

fun comb n m = (
    print ("n is ");
    print(Int.toString(n));
    print("\n");
    print ("m is ");
    print(Int.toString(m));
    print("\n");
    print(Int.toString(factorial n div (factorial m * factorial (n-m))))
    );


fun makeList 1 = "X"
| makeList n = makeList(n - 1) ^ makeList(n - 1);

fun printXs(n) = print(makeList(n));


val IN = TextIO.openIn("test.txt");

TextIO.inputN(IN,5) (*read 5 chars*)
TextIO.inputLine(IN)
TextIO.input(IN) (*whole file*)
TextIO.lookahead(IN) (*look at next char*)
TextIO.input1(IN)


fun whiteSpace(" ") = true
| whiteSpace ("\n") = true
| whiteSpace("\t") = true
| whiteSpace(_) = false
;

fun getWord(file) = if TextIO.endofStream(file) then "" 
else
let 
val c = TextIO.input1(file)
in
if whiteSpace(c) then ""
else getWord(file)
end;

fun getList(file) =
if TextIO.endofStream(file) then []
else
getWord(file) :: getList(file)



(*EXCEPTIONS*)

(*define a new exception*)
exception Foo;
raise Foo; (*raise*)

exception BadN
exception BadM

fun comb(n,m) = if n < 0 then raise BadN
else
if m < 0 orelse m > n then raise BadM
else 
if m = 0 orelse m = n then 1
else
comb(n - 1, m) + comb(n -1, m - 1)
;

exception Foo of string


exception OutOfRange of int * int
fun comb1(n,m) = if n <= 0 then raise OutOfRange(n,m)
else
if m<0 orelse m > n then raise OutOfRange(n,m)
else
if m=0 orelse m=1 then 1
else comb(n - 1, m) + (n - 1, m - 1)
;

(**Handler*)
fun comb(n,m) = comb1(n,m) handle
OutOfRange(0,0) => 1 |
OutOfRange(n,m) => (
    print("Out of Range: n=");
    print(Int.toString(n));
    ....

)


exception listTooShort
fun getThird L = if lenght(L) < 3 then raise shortList
else hd(tl(tl(L)));

(*Exception Handler for getThird*)
fun getThird1 L = getList L handle
shortList L => (
    print("List is too short");
    0
)



 fun fact1 n = if n = 0 then 1 
 else 
 if n < 0 then raise Negative(n) 
 else
 n * fact1(n - 1);

 fun fact n = fact1 n handle
 Negative n => (
 print("Negative argument");
 print(Int.toString(n));
 0
 );


 (*Polimorphic fn*)

 