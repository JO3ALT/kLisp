edge(a,b).
edge(a,c).
edge(b,d).
edge(c,e).
edge(d,f).

path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y).

?- path(a,Y).
