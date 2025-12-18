% Sudoku Solver in SWI-Prolog
% Brute-force backtracking algorithm matching C reference exactly

:- dynamic(puzzle/3).  % puzzle(Row, Col, Value)
:- dynamic(count/1).   % Iteration counter

% Initialize counter
init_count :- retractall(count(_)), assertz(count(0)).

% Increment counter
inc_count :-
    retract(count(N)),
    N1 is N + 1,
    assertz(count(N1)).

% Get current count
get_count(N) :- count(N).

% Clear puzzle
clear_puzzle :- retractall(puzzle(_, _, _)).

% Print puzzle in C format
print_puzzle :-
    nl, write('Puzzle:'), nl,
    forall(between(0, 8, R),
           (forall(between(0, 8, C),
                   (puzzle(R, C, V), format('~d ', [V]))),
            nl)).

% Read matrix from file
read_matrix(Filename) :-
    clear_puzzle,
    % Print filename (normalize /app/Matrices to ../Matrices)
    (   atom_concat('/app/Matrices/', Rest, Filename)
    ->  format('../Matrices/~w~n', [Rest])
    ;   format('~w~n', [Filename])
    ),
    open(Filename, read, Stream),
    read_rows(Stream, 0),
    close(Stream).

read_rows(Stream, 9) :- !.
read_rows(Stream, Row) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   (   (sub_string(Line, 0, 1, _, "#") ; Line == "")
        ->  read_rows(Stream, Row)  % Skip comments and empty lines
        ;   split_string(Line, " ", " ", Parts),
            exclude(=(""), Parts, Numbers),
            parse_row(Numbers, Row, 0),
            Row1 is Row + 1,
            read_rows(Stream, Row1)
        )
    ).

parse_row([], _, 9) :- nl, !.  % End of row, print newline
parse_row([], _, _) :- !.
parse_row([NumStr|Rest], Row, Col) :-
    Col < 9,
    number_string(Num, NumStr),
    assertz(puzzle(Row, Col, Num)),
    format('~d ', [Num]),
    Col1 is Col + 1,
    parse_row(Rest, Row, Col1).

% Check if value is valid at position
is_valid(Row, Col, Val) :-
    % Check row
    \+ (between(0, 8, C), C \= Col, puzzle(Row, C, Val)),
    % Check column
    \+ (between(0, 8, R), R \= Row, puzzle(R, Col, Val)),
    % Check 3x3 box
    BoxRow is (Row // 3) * 3,
    BoxCol is (Col // 3) * 3,
    BoxRowEnd is BoxRow + 2,
    BoxColEnd is BoxCol + 2,
    \+ (between(BoxRow, BoxRowEnd, BR),
        between(BoxCol, BoxColEnd, BC),
        (BR \= Row ; BC \= Col),
        puzzle(BR, BC, Val)).

% Find first empty cell (row-major order)
find_empty(Row, Col) :-
    between(0, 8, Row),
    between(0, 8, Col),
    puzzle(Row, Col, 0),
    !.

% Solve the puzzle
solve :-
    (   find_empty(Row, Col)
    ->  % Try values 1-9
        between(1, 9, Val),
        inc_count,
        (   is_valid(Row, Col, Val)
        ->  retract(puzzle(Row, Col, 0)),
            assertz(puzzle(Row, Col, Val)),
            (   solve
            ->  true
            ;   retract(puzzle(Row, Col, Val)),
                assertz(puzzle(Row, Col, 0)),
                fail
            )
        ;   fail
        )
    ;   % No empty cell - puzzle is solved
        print_puzzle,
        nl,
        get_count(Count),
        format('Solved in Iterations=~d~n~n', [Count])
    ).

% Main entry point
main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [Filename|_]
    ->  init_count,
        read_matrix(Filename),
        print_puzzle,
        (   solve
        ->  true
        ;   write('No solution found'), nl
        )
    ;   write('Usage: swipl -q -t main -s sudoku.pl -- <matrix_file>'), nl,
        halt(1)
    ),
    halt.

:- main.
