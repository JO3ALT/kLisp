parent(ann,bob).
parent(ann,dina).
parent(bob,cid).
parent(dina,emi).

grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

?- grandparent(ann,Z).
