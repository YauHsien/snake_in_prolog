:- module(snake, [ snake/5,
                   head/2,
                   move_head/3,
                   move_tail/2,
                   collide_self/1,
                   collide/2,
                   eats/2
                 ]).

snake(SnakeX, SnakeY, SnakeDir, SnakeLen, snake(Body)) :-
    member(SnakeDir, [up,down,left,right]),
    !,
    body(SnakeX-SnakeY, SnakeDir, SnakeLen, Body).

head(snake([Head|_]), Head).

body(X-Y, up, L, [X-Y,X-Y2]) :-
    Y2 is Y + L - 1.
body(X-Y, down, L, [X-Y,X-Y2]) :-
    Y2 is Y - L + 1.
body(X-Y, left, L, [X-Y,X2-Y]) :-
    X2 is X + L - 1.
body(X-Y, right, L, [X-Y,X2-Y]) :-
    X2 is X - L + 1.

move_head(snake([X-Y|R]), D, snake([X2-Y2|R])) :-
    move(X-Y, D, X2-Y2).

move_tail(snake(S), snake(S2)) :-
    append(L, [X0-Y0,X-Y], S),
    dir([X0-Y0,X-Y], D),
    move(X-Y, D, X2-Y2),
    ( X0-Y0 == X2-Y2, !,
      append(L, [X0-Y0], S2)
    ; append(L, [X0-Y0,X2-Y2], S2)
    ).

dir([X-Y,X-Z], up) :- Y < Z.
dir([X-Y,X-Z], down) :- Y > Z.
dir([X-Y,Z-Y], left) :- X < Z.
dir([X-Y,Z-Y], right) :- X > Z.

move(X-Y, up, X-Z) :- Z is Y - 1.
move(X-Y, down, X-Z) :- Z is Y + 1.
move(X-Y, left, Z-Y) :- Z is X - 1.
move(X-Y, right, Z-Y) :- Z is X + 1.

collide_self(Snake) :-
    snake(S) = Snake,
    head(Snake, Head),
    at(Head, S).

collide(Snake, Lines) :-
    snake(_) = Snake,
    head(Snake, Head),
    at(Head, Lines).

eats(snake(Snake), eatable(_,I)) :-
    head(snake(Snake), I).

at(X-Y, [X-Y0,X-Y2]) :- Y0 =< Y, Y =< Y2.
at(X-Y, [X0-Y,X2-Y]) :- X0 =< X, X =< X2.
at(X-Y, [A,B,_|_]) :-
    at(X-Y, [A,B]), !.
at(X-Y, [_,B,C|R]) :-
    at(X-Y, [B,C|R]).
