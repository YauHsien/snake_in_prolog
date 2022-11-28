:- module(snake, [snake/5]).

snake(SnakeX, SnakeY, SnakeDir, SnakeLen, snake(Body)) :-
    member(SnakeDir, [up,down,left,right]),
    !,
    body(SnakeX-SnakeY, SnakeDir, SnakeLen, [], Body).

body(_-_, _, 0, B, B) :- !.
body(X-Y, D, L, B, R) :-
    L > 0,
    !,
    next(X-Y, D, X2-Y2),
    L2 is L - 1,
    append(B, [X2-Y2], B2),
    body(X2-Y2, D, L2, B2, R).

next(X-Y, up, X-Y2) :- Y2 is Y - 1.
next(X-Y, down, X-Y2) :- Y2 is Y + 1.
next(X-Y, left, X2-Y) :- X2 is X - 1.
next(X-Y, right, X2-Y) :- X2 is X + 1.
