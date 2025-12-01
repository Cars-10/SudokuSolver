% Sudoku Solver in Prolog (Explicit Backtracking)
% Matches C algorithm for benchmarking

:- dynamic iteration_count/1.

% Main entry point
sudoku(Rows, SolvedRows) :-
    retractall(iteration_count(_)),
    assert(iteration_count(0)),
    solve(Rows, SolvedRows).

% Solve: Find empty cell, try numbers, recurse
solve(Rows, SolvedRows) :-
    find_empty(Rows, RowIdx, ColIdx),
    !, % Found an empty cell, commit to it
    between(1, 9, Num),
    increment_counter,
    is_valid(Rows, RowIdx, ColIdx, Num),
    replace_in_matrix(Rows, RowIdx, ColIdx, Num, NewRows),
    solve(NewRows, SolvedRows).
solve(Rows, Rows). % No empty cells, solved! Return current board.

% Find first empty cell (0)
% Returns 0-based indices
find_empty(Rows, RowIdx, ColIdx) :-
    nth0(RowIdx, Rows, Row),
    nth0(ColIdx, Row, 0),
    !.

% Check if placing Num at (RowIdx, ColIdx) is valid
is_valid(Rows, RowIdx, ColIdx, Num) :-
    % Row check
    nth0(RowIdx, Rows, Row),
    \+ member(Num, Row),
    
    % Column check
    check_col(Rows, ColIdx, Num),
    
    % Box check
    BoxRowStart is (RowIdx // 3) * 3,
    BoxColStart is (ColIdx // 3) * 3,
    check_box(Rows, BoxRowStart, BoxColStart, Num).

% Check column
check_col([], _, _).
check_col([Row|Rows], ColIdx, Num) :-
    nth0(ColIdx, Row, Val),
    Val \= Num,
    check_col(Rows, ColIdx, Num).

% Check 3x3 box
check_box(Rows, StartRow, StartCol, Num) :-
    between(0, 2, R),
    between(0, 2, C),
    RIdx is StartRow + R,
    CIdx is StartCol + C,
    nth0(RIdx, Rows, Row),
    nth0(CIdx, Row, Val),
    Val = Num,
    !, fail.
check_box(_, _, _, _).

% Replace value in matrix (non-destructive, creates new matrix)
replace_in_matrix(Rows, RowIdx, ColIdx, Val, NewRows) :-
    nth0(RowIdx, Rows, Row, RestRows),
    nth0(ColIdx, Row, _, RestRow),
    nth0(ColIdx, NewRow, Val, RestRow),
    nth0(RowIdx, NewRows, NewRow, RestRows).

% Counter
increment_counter :-
    retract(iteration_count(N)),
    N1 is N + 1,
    assert(iteration_count(N1)).

% Read matrix from file
read_matrix(File, Rows) :-
    open(File, read, Stream),
    read_lines(Stream, Rows),
    close(Stream).

read_lines(Stream, Rows) :-
    read_line_to_string(Stream, String),
    (   String == end_of_file
    ->  Rows = []
    ;   string_codes(String, Codes),
        (   Codes = []
        ->  read_lines(Stream, Rows)
        ;   Codes = [35|_] % 35 is '#'
        ->  read_lines(Stream, Rows)
        ;   split_string(String, " ", " ", Parts),
            (   Parts = []
            ->  read_lines(Stream, Rows)
            ;   maplist(number_string, Row, Parts),
                Rows = [Row|Rest],
                read_lines(Stream, Rest)
            )
        )
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
    (   sudoku(Rows, SolvedRows)
    ->  writeln('Puzzle:'),
        print_puzzle(SolvedRows),
        iteration_count(Count),
        format('Solved in Iterations=~w~n', [Count])
    ;   writeln('No solution found.')
    ),
    halt(0).
