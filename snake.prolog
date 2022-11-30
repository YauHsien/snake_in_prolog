:- module(snake, [ snake/5,
                   head/2
                 ]).

snake(SnakeX, SnakeY, SnakeDir, SnakeLen, snake(Body)) :-
    member(SnakeDir, [up,down,left,right]),
    !,
    body(SnakeX-SnakeY, SnakeDir, SnakeLen, Body).

head(snake([Head|_]), Head).

body(X-Y, up, L, [X-Y,X-Y2]) :-
    Y2 is Y - L + 1.
body(X-Y, down, L, [X-Y,X-Y2]) :-
    Y2 is Y + L - 1.
body(X-Y, left, L, [X-Y,X2-Y]) :-
    X2 is X - L + 1.
body(X-Y, right, L, [X-Y,X2-Y]) :-
    X2 is X + L - 1.
