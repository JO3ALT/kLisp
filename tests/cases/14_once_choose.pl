p(a).
p(b).
q(b).

choose(X) :- p(X), q(X).

?- once(choose(X)).
