
subbags([], [[]]).
%subbags([X|XS], P) :- ..

addOneToAll(_, [], []).
%addOneToAll(E, [L|LS], [[E|L]|T]) :- ...





:- dynamic robot/2, dira/1.

robot(42,1).
robot(99,5).
dira(0).
dira(3).


obsazeno(P) :- robot(_,P); dira(P).
vytvor(I, P) :- not(obsazeno(P)), assertz(robot(I,P)).
vytvor(P) :- not(obsazeno(P)), assertz(dira(P)).

odstran(P) :- retract(dira(P)); retract(robot(_,P)).

obsazene_pozice(X) :- bagof(XS,obsazeno(XS),X).
obsazene_roboty(X) :- bagof(XS,Y^robot(Y,XS),X).

inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.
doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).
pohni(I, Operace) :- robot(I, X), odstran(X), call(Operace, X, Y), vytvor(I,Y). 

armageddon :- forall(robot(_,P),vybuch(P)).
vybuch(P) :- odstran(P), vytvor(P). 









g_size(3).


g_test(X:Y) :- g_size(S), X > 0, Y > 0, X =< S, Y =< S.



g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).



g_one(X:Y, Len, L, R) :- reverse([X:Y|L], R), length(R, Len).
g_one(X:Y, Len, L, R) :- g_move(X:Y,X2:Y2), not(memberchk(X2:Y2,L)), g_one(X2:Y2, Len, [X:Y|L], R).



g_all(R, Len) :- g_size(S), between(1,S,X), between(1,S,Y), g_one(X:Y, Len, [], R).
%g_all(R, Len) :-



g_allLength(R) :- g_size(S), Max is S*S, between(1,Max,L), g_all(R,L).
/*
g_allLength(R, Len) :- 
g_allLength(R, Len) :- 
*/

