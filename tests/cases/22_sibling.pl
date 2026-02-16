parent(tom,bob).
parent(tom,liz).
parent(tom,ken).
neq(bob,liz).
neq(liz,bob).
neq(bob,ken).
neq(ken,bob).
neq(liz,ken).
neq(ken,liz).

sibling(X,Y) :- parent(P,X), parent(P,Y), neq(X,Y).

?- sibling(bob,Y).
