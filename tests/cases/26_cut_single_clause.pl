p(a).
q(a).
q(b).

choose(X) :- p(X), !, q(X).

?- choose(X).
