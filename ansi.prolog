:- module(ansi, [command/2, command/3, command/4]).

command(cursor-up, N, Command) :-
    ansi(N, "A", Command).
command(cursor-down, N, Command) :-
    ansi(N, "B", Command).
command(cursor-right, N, Command) :-
    ansi(N, "C", Command).
command(cursor-left, N, Command) :-
    ansi(N, "D", Command).
command(clear-screen, eos, Command) :-
    ansi(0, "J", Command).
command(clear-screen, bos, Command) :-
    ansi(1, "J", Command).
command(clear-screen, entire, Command) :-
    ansi(2, "J", Command).
command(clear-line, eol, Command) :-
    ansi(0, "K", Command).
command(clear-line, bol, Command) :-
    ansi(1, "K", Command).
command(clear-line, entire, Command) :-
    ansi(2, "K", Command).

command(cursor-position, R, C, Command) :-
    ansi(R, C, "H", Command).

command(color-reset, Command) :-
    ansi(0, "m", Command).
command(color-black, Command) :-
    ansi(30, "m", Command).
command(color-red, Command) :-
    ansi(31, "m", Command).
command(color-green, Command) :-
    ansi(32, "m", Command).
command(color-yellow, Command) :-
    ansi(33, "m", Command).
command(color-blue, Command) :-
    ansi(34, "m", Command).
command(color-magenta, Command) :-
    ansi(35, "m", Command).
command(color-cyan, Command) :-
    ansi(36, "m", Command).
command(color-white, Command) :-
    ansi(37, "m", Command).
command(color-blackb, Command) :-
    ansi(30, 1, "m", Command).
command(color-redb, Command) :-
    ansi(31, 1, "m", Command).
command(color-greenb, Command) :-
    ansi(32, 1, "m", Command).
command(color-yellowb, Command) :-
    ansi(33, 1, "m", Command).
command(color-blueb, Command) :-
    ansi(34, 1, "m", Command).
command(color-magentab, Command) :-
    ansi(35, 1, "m", Command).
command(color-cyanb, Command) :-
    ansi(36, 1, "m", Command).
command(color-whiteb, Command) :-
    ansi(37, 1, "m", Command).
command(color-bblack, Command) :-
    ansi(40, "m", Command).
command(color-bred, Command) :-
    ansi(41, "m", Command).
command(color-bgreen, Command) :-
    ansi(42, "m", Command).
command(color-byellow, Command) :-
    ansi(43, "m", Command).
command(color-bblue, Command) :-
    ansi(44, "m", Command).
command(color-bmagenta, Command) :-
    ansi(45, "m", Command).
command(color-bcyan, Command) :-
    ansi(46, "m", Command).
command(color-bwhite, Command) :-
    ansi(47, "m", Command).
command(color-bblackb, Command) :-
    ansi(40, 1, "m", Command).
command(color-bredb, Command) :-
    ansi(41, 1, "m", Command).
command(color-bgreenb, Command) :-
    ansi(42, 1, "m", Command).
command(color-byellowb, Command) :-
    ansi(43, 1, "m", Command).
command(color-bblueb, Command) :-
    ansi(44, 1, "m", Command).
command(color-bmagentab, Command) :-
    ansi(45, 1, "m", Command).
command(color-bcyanb, Command) :-
    ansi(46, 1, "m", Command).
command(color-bwhiteb, Command) :-
    ansi(47, 1, "m", Command).
command(color-N, Command) :-
    between(0, 255, N),
    ansi(38, 5, N, "m", Command).
command(bgcolor-N, Command) :-
    between(0, 255, N),
    ansi(48, 5, N, "m", Command).
command(decoration-bold, Command) :-
    ansi(1, "m", Command).
command(decoration-underline, Command) :-
    ansi(4, "m", Command).
command(decoration-reversed, Command) :-
    ansi(7, "m", Command).

ansi(N, S, Command) :-
    number_string(N, Sn),
    string_concat("\x1b[", Sn, C0),
    string_concat(C0, S, Command).

ansi(N, M, S, Command) :-
    number_string(N, Sn),
    number_string(M, Sm),
    string_concat("\x1b[", Sn, C0),
    string_concat(C0, ";", C2),
    string_concat(C2, Sm, C3),
    string_concat(C3, S, Command).

ansi(N, M, X, S, Command) :-
    number_string(N, Sn),
    number_string(M, Sm),
    number_string(X, Sx),
    string_concat("\x1b[", Sn, C0),
    string_concat(C0, ";", C2),
    string_concat(C2, Sm, C3),
    string_concat(C3, ";", C4),
    string_concat(C4, Sx, C5),
    string_concat(C5, S, Command).
