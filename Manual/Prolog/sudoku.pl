% Sudoku Solver in Prolog
% Uses constraint logic programming

:- use_module(library(clpfd)).

% Global counter for iterations
:- dynamic iteration_count/1.

sudoku(Rows) :-
    retractall(iteration_count(_)),
    assert(iteration_count(0)),
    length(Rows, 9), maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

% Custom labeling that counts iterations
label_sudoku([]).
label_sudoku([V|Vs]) :-
    (   var(V)
    ->  (   between(1, 9, V),
            increment_counter,
            label_sudoku(Vs)
        )
    ;   label_sudoku(Vs)
    ).

increment_counter :-
    retract(iteration_count(N)),
    N1 is N + 1,
    assert(iteration_count(N1)).

% Read matrix from file
read_matrix(File, Rows) :-
    open(File, read, Stream),
    read_lines(Stream, Rows),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Row|Rows]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    (   string_chars(String, [C|_]), C \= '#'
    ->  split_string(String, " ", " ", Parts),
        maplist(number_string, Row, Parts),
        read_lines(Stream, Rows)
    ;   read_lines(Stream, [Row|Rows])
    ).

% Print puzzle
print_puzzle([]).
print_puzzle([Row|Rows]) :-
    print_row(Row),
    nl,
    print_puzzle(Rows).

print_row([]).
print_row([N|Ns]) :-
    write(' '), write(N), write(' '),
    print_row(Ns).

% Main
main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [File|_]
    ->  true
    ;   writeln('Usage: swipl -q -t main -s sudoku.pl -- input_file'),
        halt(1)
    ),
    writeln(File),
    read_matrix(File, Rows),
    writeln('Puzzle:'),
    print_puzzle(Rows),
    sudoku(Rows),
    append(Rows, Vs),
    label_sudoku(Vs),
    writeln('Puzzle:'),
    print_puzzle(Rows),
    iteration_count(Count),
    format('Solved in Iterations=~w~n', [Count]),
    halt(0).
