(****Exam type question*)
(****write function suffixes*)

fun suffixes "" = nil
|suffixes str = str :: suffixes(implode(tl(explode(str)))); 



fun get_words Empty = ("", "") |
 get_words(Leaf c) = (str c, "")|
 get_words(Node(r,left,right)) = 
 let
 val (leftLeaves, leftNodes) = get_words(left);
 val (rLeaves,rNodes) = get_words(right);
 in
 (leftLeaves ^ rLeaves, str r ^ leftNodes ^ rNodes)
 end;