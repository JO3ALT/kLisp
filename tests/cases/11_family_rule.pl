parent(ann,bob).
parent(bob,cid).
parent(ann,dina).

grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

?- grandparent(ann,Z).
