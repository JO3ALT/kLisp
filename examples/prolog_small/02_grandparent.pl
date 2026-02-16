parent(ann,bob).
parent(bob,cid).
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).
?- grandparent(ann,Z).
