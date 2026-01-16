#!/usr/bin/env swipl
% CP (Constraint Propagation) Sudoku Solver - Prolog Implementation
% Using CLPFD with MRV heuristic and iteration counting

:- use_module(library(clpfd)).

:- dynamic(cp_iterations/1).
:- dynamic(puzzle/3).

% Initialize iteration counter
init_counter :- retractall(cp_iterations(_)), assertz(cp_iterations(0)).

% Increment counter
inc_counter :-
    retract(cp_iterations(N)),
    N1 is N + 1,
    assertz(cp_iterations(N1)).

% Sudoku constraints
sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
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

% Custom labeling with MRV heuristic and iteration counting
label_mrv([]).
label_mrv(Vars) :-
    select_mrv(Vars, Var, Rest),
    inc_counter,
    indomain(Var),
    label_mrv(Rest).

% Select variable with minimum remaining values
select_mrv([V|Vs], Selected, Rest) :-
    fd_size(V, Size),
    select_mrv(Vs, V, Size, Selected, [], Rest).

select_mrv([], Best, _, Best, Acc, Acc).
select_mrv([V|Vs], CurBest, CurSize, Selected, Acc, Rest) :-
    fd_size(V, Size),
    (Size < CurSize
    -> select_mrv(Vs, V, Size, Selected, [CurBest|Acc], Rest)
    ; select_mrv(Vs, CurBest, CurSize, Selected, [V|Acc], Rest)).

% Build rows from puzzle facts (0 becomes free variable)
build_rows(Rows) :-
    findall(Row,
            (between(0, 8, R),
             findall(V, (between(0, 8, C), puzzle(R, C, PV), (PV =:= 0 -> true ; V = PV)), Row)),
            Rows).

% Solve puzzle
solve_cp :-
    build_rows(Rows),
    sudoku(Rows),
    append(Rows, Flat),
    include(var, Flat, Vars),
    label_mrv(Vars),
    % Update puzzle facts with solution
    update_puzzle(Rows, 0).

update_puzzle([], _).
update_puzzle([Row|Rows], R) :-
    update_row(Row, R, 0),
    R1 is R + 1,
    update_puzzle(Rows, R1).

update_row([], _, _).
update_row([V|Vs], R, C) :-
    retract(puzzle(R, C, _)),
    assertz(puzzle(R, C, V)),
    C1 is C + 1,
    update_row(Vs, R, C1).

% Print puzzle
print_puzzle :-
    nl, write('Puzzle:'), nl,
    forall(between(0, 8, R),
           (forall(between(0, 8, C),
                   (puzzle(R, C, V), format('~d ', [V]))),
            nl)).

% Read matrix from file
read_matrix(Filename) :-
    retractall(puzzle(_, _, _)),
    % Normalize path
    (atom_concat('/app/Matrices/', Rest, Filename)
    -> format('../Matrices/~w~n', [Rest])
    ; format('~w~n', [Filename])),

    open(Filename, read, Stream),
    read_rows(Stream, 0),
    close(Stream).

read_rows(Stream, 9) :- !.
read_rows(Stream, Row) :-
    read_line_to_string(Stream, Line),
    (Line == end_of_file
    -> true
    ; (sub_string(Line, 0, 1, _, "#") ; Line == "")
    -> read_rows(Stream, Row)
    ; split_string(Line, " \t", " \t", Parts),
      exclude(=(""), Parts, Numbers),
      (length(Numbers, 9)
      -> parse_row(Numbers, Row, 0),
         Row1 is Row + 1,
         read_rows(Stream, Row1)
      ; read_rows(Stream, Row))).

parse_row([], _, 9) :- nl, !.
parse_row([], _, _) :- !.
parse_row([NumStr|Rest], Row, Col) :-
    Col < 9,
    number_string(Num, NumStr),
    assertz(puzzle(Row, Col, Num)),
    format('~d ', [Num]),
    Col1 is Col + 1,
    parse_row(Rest, Row, Col1).

% Main entry point
main :-
    current_prolog_flag(argv, Argv),
    get_matrix_file(Argv, Filename),
    (Filename \= ''
    -> init_counter,
       read_matrix(Filename),
       print_puzzle,
       (solve_cp
       -> print_puzzle,
          cp_iterations(Count),
          format('~nSolved in Iterations=~d~n~n', [Count])
       ; write('No solution found'), nl)
    ; write('Usage: swipl -q -t main -s cp.pl -- <matrix_file>'), nl),
    halt.

get_matrix_file([], '').
get_matrix_file([Arg|_], Arg) :- atom_string(Arg, S), sub_string(S, _, _, 0, ".matrix"), !.
get_matrix_file([_|Rest], File) :- get_matrix_file(Rest, File).

:- initialization(main, main).
