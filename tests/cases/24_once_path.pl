edge(a,b).

path(X,Y) :- edge(X,Y).

?- once(path(a,Y)).
