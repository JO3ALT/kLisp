parent(ann,bob).
parent(bob,cid).
parent(cid,dan).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
?- ancestor(ann,Y).
