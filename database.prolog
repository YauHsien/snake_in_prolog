:- module(database, [update_db/2]).

update_db(Goal0, Goal) :-
    retract(Goal0), !,
    assertz(Goal).
