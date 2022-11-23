:- module(input, [keystroke/1]).

keystroke(Key) :-
    prompt1(''),
    with_tty_raw(
        ( get_char(C),
          read_pending_codes(user_input, Cs, [])
        )),
    key_int([C|Cs], Key), !.

key_int(['\u0010'], up).
key_int(['\u0002'], left).
key_int(['\u000E'], down).
key_int(['\u0006'], right).
key_int([q], quit).
