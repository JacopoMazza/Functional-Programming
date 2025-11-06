(****2-3 Trees, each node ha either 2 or 3 children*)

structure TwoThreeTree = struct

datatype Tree = 
Leaf of Int
| TwoTree of Tree * int * Tree
|ThreeTree of Tree * int * Tree * int * Tree;
(***int is the separator*)


fun create x = Leaf x;

datatype InsertIntermediate = 
S of Tree | P of Tree * int * Tree


fun insert1(Leaf x, a) = 
if a < x then P(Leaf a, x , Leaf x)
else P(Leaf x, a , Leaf a);

| insert1(TwoTree(L,x,R),a) = 
if a < x
        then 
            


fun 
