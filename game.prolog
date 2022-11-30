:- use_module([ 'command.prolog',
                'database.prolog',
                'input.prolog',
                'snake.prolog'
              ]).

game :-
    config_game,
    draw_map,
    interval_seconds(Int),
    thread_create(update_game(Int), _),
    sleep(0.05),
    thread_create(write_update(Int), _),
    keep_input,
    reset_game.

config_game :-
    command:map(C, R),
    SnakeX is C div 2,
    SnakeY is R div 2,
    random_member(SnakeDir, [up,down,left,right]),
    SnakeLen = 3,
    snake:snake(SnakeX, SnakeY, SnakeDir, SnakeLen, Snake),
    config([ interval_seconds(0.5),
             game_name('Snake! Arrow Keys: Turn Way; Q Key: Quit . . . '),
             game_map(C, R),
             snake_dir(SnakeDir),
             Snake,
             new_snake(Snake),
             eatable(+, 1-1),
             game_start(true)
           ]).

config(List) :-
    foreach( member(Term, List),
             asserta(Term) ).

reset_game :-
    reset_config([ interval_seconds(_),
                   game_name(_),
                   game_map(_, _),
                   snake_dir(_),
                   snake(_),
                   new_snake(_),
                   game_start(_)
                 ]),
    nl,
    writeln('Game End').

reset_config(List) :-
    foreach( member(Term, List),
             retract(Term) ).

draw_map :-
    game_map(C, R),
    command:expand_map(C, R, ., ~, '|', '+'),
    game_name(Name),
    write(Name).

keep_input :-
    game_start(true), !,
    input:keystroke(Key),
    update_db(Key),
    keep_input.
keep_input.

update_db(Way) :-
    member(Way-Another, [up-down,down-up,left-right,right-left]),
    snake_dir(Another), !,
    fail.
update_db(Way) :-
    member(Way, [up,down,left,right]), !,
    context_module(M),
    database:update_db(M:snake_dir(_), M:snake_dir(Way)).
update_db(quit) :-
    context_module(M),
    database:update_db(M:game_start(true), M:game_start(false)).
update_db(_).

update_game(Int) :-
    game_start(true), !,
    ( check_collision,
      move_snake_head,
      ( once(snake_eats_something), !
      ; move_snake_tail
      )
    ),
    sleep(Int),
    update_game(Int).
update_game(_).

write_update(Int) :-
    game_start(true), !,
    ( clear_snake
    , update_snake
    , draw_snake
    , rest_cursor
    , flush_output
    ),
    sleep(Int),
    write_update(Int).
write_update(_).

clear_snake :-
    snake(Snake),
    draw(Snake, .).

update_snake :-
    new_snake(Snake),
    context_module(M),
    database:update_db(M:snake(_), M:Snake).

draw_snake :-
    snake(Snake),
    draw(Snake, #),
    draw_snake_head.

draw_snake_head :-
    snake(Snake),
    snake:head(snake(Snake), Head),
    command:put(Head, '%').

rest_cursor :-
    game_map(X, Y),
    command:move_to(X, Y).

draw([I], S) :-
    command:put(I, S).
draw([I,I|R], S) :-
    draw([I|R], S).
draw([X-Y,X-Z|R], S) :-
    command:put(X-Y, S),
    next(Y, Z, A),
    draw([X-A,X-Z|R], S).
draw([X-Y,Z-Y|R], S) :-
    command:put(X-Y, S),
    next(X, Z, A),
    draw([A-Y,Z-Y|R], S).

next(X, Y, Z) :-
    X < Y,
    Z is X + 1.
next(X, Y, Z) :-
    X > Y,
    Z is X - 1.

move_snake_head :-
    snake(Snake),
    snake_dir(Dir),
    snake:move_head(snake(Snake), Dir, Snake2),
    context_module(M),
    database:update_db(M:new_snake(_), M:new_snake(Snake2)).

move_snake_tail :-
    new_snake(Snake),
    snake:move_tail(Snake, Snake2),
    context_module(M),
    database:update_db(M:new_snake(_), M:new_snake(Snake2)).

check_collision :-
    snake(Snake),
    game_map(X, Y),
    ( ( snake:collide_self(snake(Snake)), !
      ; snake:collide(snake(Snake), [1-1,1-X,X-Y,1-Y,1-1])
      ),
      context_module(M),
      database:update_db(M:game_start(true), M:game_start(false))
    ; true
    ).

snake_eats_something :-
    snake(Snake),
    eatable(S, I),
    snake:eats(snake(Snake), eatable(S,I)),
    retract(eatable(S,I)).
