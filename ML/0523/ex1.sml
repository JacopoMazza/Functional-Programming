fun averageWeight (L) = foldl Real.+ 0.0 (map (fn dino:dino => #weight (dino)) L) / Real.fromInt(List.lenght(L));


fun matchName (L: student list) name = List.filter(fn {name, ...} => n = name) L;

fun checkAll (A,i) = i < 0 orelse sub(A,i) andalso checkAll(A, i - 1);

fun inc i  =  i:= !i + 1; (****uses references*)
fun dec i = i:= !i - 1;

datatype 'a List = Nil | Cell of {data: 'a, next 'a List}

(****Exam type question*)
(****write function suffixes*)

fun suffixes "" = nil
|suffixes str = str :: suffixes(implode(tl(explode(str)))); 

