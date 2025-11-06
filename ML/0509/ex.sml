(***STACK DEFINITION*)

signature Stack = sig

val empty: 'a list
val pop: 'a list -> 'a option
val push : 'a * 'a list -> 'a list
eqtype 'a stack
    
end;

structure Stack = struct

type 'a stack = 'a list
val empty = []
val push = op ::
fun pop [] = NONE
| pop(tos:: rest) = SOME tos
end:> Stack;

(***SET DEFINITION*)
signature Set = sig
type 'a set
val emptySet: 'a set
val isIn: ''a -> ''a set -> bool
val addIn: ''a -> ''a set -> ''set
val removeFrom: ''a -> ''a set -> ''set
end;


structure Set = struct 
type 'a set = 'a list
val emptySet = []

fun isIn x [] = false
| isIn x (y::l) = if (x=y) then true else isIn x l;

fun addIn x l = 
    if (is x l) then l else x::l

fun removeFrom x [] = []
| removeFrom x (y::ys) = if (x = y) then ys else y::(removeFrom x ys);

end :> Set



(***TREE DEFINITON*)

datatype 'a T = Lf | Br of 'a * 'a T * 'a T 


signature Tree = 
sig 
datatype 'a T = Lf | Br of 'a * 'a T * 'a T | Lf
val count: 'a T -> int
val depth: 'a T -> int
val reflect: 'a T -> 'a TREE
end


structure Tree = struct
datatype 'a T = Lf | Br of 'a * 'a T * 'a T

fun count Lf = 0
| count(Br(v,t1,t2)) = 1 + count(t1) + count(t2)

fun depth Lf = 0
| depth(Br(v,t1,t2)) =  1 + Int.max(depth(t1), depth(t2))

fun reflect Lf = Lf
|reflect (Br(v,t1,t2)) = Br(v,reflect(t2), reflect(t1))

end :> Tree;


(***BINARY SEARCH TREE --> LEFT SUBTREE LESS THAN RIGHT SUBTREE*)

fun lower(nil) = nil
| lower(c::cs) = (Char.toLower c) :: lower(cs)


datatype 'a btree = Empty | Node of 'a * 'a btree * 'a btree

(***Order in the tree is given by the function lt*)
fun lookup lt Empty x = false
| lookup lt (Node(y,left,right)) x = 
    if lt(x,y) then lookup lt left x 
    else if lt(y,x) then lookup lt right x1
    else true;


fun insert lt Empty x = Node(x,Empty,Empty)
| insert lt (T as Node(y,left,right)) x = 
    if lt (x,y) then Node(y, (insert lt left x),right) 
    else if lt (y,x) then Node(y, left, (insert lt right x))
    else T

exception EmptyTree

fun deletemin (Empty) = raise EmptyTree
| deletemin (Node(y,Empty,right)) = (y,right)
| deletemin (Node(w,left,right)) =
    let val (y,L) = deletemin(left)
in (y,Node(w,L,right))
end;

fun delete lt Empty x = Empty
| delete lt (Node(y,left,right)) x = 
if lt(x,y) then Node(y,(delete lt left x), right)
else if lt (y,x) then Node(y,left,(delete lt right x))
else
    case(left, right) of
    (Empty, r) => r |
    (l,Empty) => l |
    (l,r) =>
        let val (x,r1) = deletemin(r)
        in Node(x,l,r1)
        end;



fun sum (Empty) = 0
| sum(Node(a,left,right)) = a + sum(left) + sum(right)

fun preOrder(Empty) = nil
| preOrder(Node(a,left,right)) = [a] @ preOrder(left) @ preOrder(right)

fun postOrder(Empty) = nil
| postOrder(Node(a,left,right)) = postOrder(left) @ postOrder(right) @ [a]

fun inOrder(Empty) = nil
| inOrder(Node(a,left,right)) = inOrder(left) @ [a] @ inOrder(right)


type ('d,'r) mapTree = ('d * 'r) btree


(****Look for ae ntry label (a,b) in tree and return (a,b)*******)

exception Missing;

fun lookup lt Empty a = raise Missing | 
lookup lt (Node((c,b),left,right)) a = if lt(a,c) then lookup lt left a 
else if lt(c,a) then lookup lt right a
else b

(*****ASSIGN --> looks for pair a,b if found modifies to a,c otherwise adds it *)
fun assign lt Empty a b = Node((a,b), Empty, Empty) |
assign lt ((Node(k,v), L, R)) a b = if lt(a,k) then Node((k,v), assign lt L a b, R)
else if lt(k,a) then Node((k,v), L, assign lt R a b)
else Node((k,b), L, R);


fun ltpair((a,b), (c,d)) = if lt (a,c) then true 
else if lt(c,a) then false 
else lt(b,d);

