:- use_module([ 'command.prolog',
                'database.prolog',
                'input.prolog',
                'snake.prolog'
              ]).

game :-
    config_game,
    draw_map,
    interval_seconds(Int),
    thread_create(write_update(Int), _Id),
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

write_update(Int) :-
    game_start(true), !,
    clear_snake,
    update_snake,
    draw_snake,
    rest_cursor,
    flush_output,
    sleep(Int),
    write_update(Int).
write_update(_).

clear_snake :-
    snake(Snake),
    foreach(member(X-Y, Snake),
            command:put(X-Y, .)).

update_snake :-
    new_snake(Snake),
    context_module(M),
    database:update_db(M:snake(_), M:Snake).

draw_snake :-
    snake(Snake),
    foreach(member(X-Y, Snake),
            command:put(X-Y, #)),
    draw_snake_head.

draw_snake_head :-
    snake([X-Y|_]),
    command:put(X-Y, '%').

rest_cursor :-
    game_map(X, Y),
    command:move_to(X, Y).
