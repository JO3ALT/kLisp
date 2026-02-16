works_at(alice,acme).
works_at(bob,acme).
works_at(caro,globex).
neq(alice,bob).
neq(bob,alice).
colleague(X,Y) :- works_at(X,C), works_at(Y,C), neq(X,Y).
?- colleague(alice,Y).
