:- module(command, [ clear_screen/0,
                     expand_map/6,
                     map/2,
                     move_to/2
                   ]).
:- use_module(ansi).

clear_screen :-
    ansi:command(clear-screen, entire, Cmd),
    write(Cmd).

expand_map(C, R, S, Bh, Bv, Bc) :-
    clear_screen,
    foreach( between(1, R, N),
             ( foreach( between(1, C, M),
                        ( member(N, [1,R]),
                          member(M, [1,C]),
                          !,
                          write(Bc)
                        ; ( 1 =:= N, !
                          ; R =:= N
                          ), !,
                          write(Bh)
                        ; ( 1 =:= M, !
                          ; C =:= M
                          ), !,
                          write(Bv)
                        ; write(S)
                        ) )
             , nl
             )).

map(C, R) :-
    tty_size(R0, C0),
    R is R0 - 1,
    C is C0 - 1.

move_to(C, R) :-
    ansi:command(cursor-position, R, C, Cmd),
    write(Cmd).
