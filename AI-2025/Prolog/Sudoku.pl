:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(main)).

main(Args) :-
    get_time(Start),
    process_files(Args),
    get_time(End),
    Elapsed is End - Start,
    format("Seconds to process ~3f~n", [Elapsed]),
    halt.

process_files([]).
process_files([File|Rest]) :-
    (sub_atom(File, _, _, _, '.matrix') ->
        process_file(File)
    ;
        true
    ),
    process_files(Rest).

process_file(File) :-
    format("~w~n", [File]),
    read_matrix_file(File, Rows),
    print_puzzle(Rows),
    (solve(Rows) ->
        print_puzzle(Rows),
        format("Solved in Iterations=0~n~n") % CLP(FD) doesn't easily give iteration count
    ;
        format("No solution found~n")
    ).

read_matrix_file(File, Rows) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r", Lines),
    parse_lines(Lines, Rows).

parse_lines([], []).
parse_lines([Line|Rest], Rows) :-
    (sub_string(Line, 0, 1, _, "#") ->
        parse_lines(Rest, Rows)
    ;
        split_string(Line, " \t", " \t", Parts),
        length(Parts, 9) ->
            maplist(number_string, RowNums, Parts),
            maplist(zero_to_var, RowNums, Row),
            Rows = [Row|RestRows],
            parse_lines(Rest, RestRows)
    ;
        parse_lines(Rest, Rows)
    ).

zero_to_var(0, _) :- !.
zero_to_var(N, N).

print_puzzle(Rows) :-
    format("~nPuzzle:~n"),
    maplist(print_row, Rows).

print_row(Row) :-
    maplist(var_to_zero, Row, OutRow),
    format("~w ~w ~w ~w ~w ~w ~w ~w ~w~n", OutRow).

var_to_zero(X, 0) :- var(X), !.
var_to_zero(X, X).

solve(Rows) :-
    append(Rows, Vs),
    Vs ins 1..9,
    Rows = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    all_distinct(R1), all_distinct(R2), all_distinct(R3),
    all_distinct(R4), all_distinct(R5), all_distinct(R6),
    all_distinct(R7), all_distinct(R8), all_distinct(R9),
    transpose(Rows, Cols),
    Cols = [C1,C2,C3,C4,C5,C6,C7,C8,C9],
    all_distinct(C1), all_distinct(C2), all_distinct(C3),
    all_distinct(C4), all_distinct(C5), all_distinct(C6),
    all_distinct(C7), all_distinct(C8), all_distinct(C9),
    blocks(R1, R2, R3), blocks(R4, R5, R6), blocks(R7, R8, R9),
    label(Vs).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    blocks(Bs1, Bs2, Bs3).
