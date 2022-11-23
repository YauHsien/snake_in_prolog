:- use_module([ 'command.prolog',
                'database.prolog',
                'input.prolog'
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
    config([ interval_seconds(0.5),
             game_name('Snake! Arrow Keys: Turn Way; Q Key: Quit . . . '),
             game_map(C, R),
             game_start(true)
           ]).

config(List) :-
    foreach( member(Term, List),
             asserta(Term) ).

reset_game :-
    reset_config([ interval_seconds(_),
                   game_name(_),
                   game_map(_, _),
                   game_start(_)
                 ]),
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

update_db(up).
update_db(down).
update_db(right).
update_db(left).
update_db(quit) :-
    context_module(M),
    database:update_db(M:game_start(true),
                       M:game_start(false)).

write_update(Int) :-
    game_start(true), !,
    flush_output,
    sleep(Int),
    write_update(Int).
write_update(_).
