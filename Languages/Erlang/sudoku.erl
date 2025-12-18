#!/usr/bin/env escript
%% -*- erlang -*-
%% Sudoku Solver in Erlang
%% Brute-force backtracking algorithm matching C reference exactly

main([Filename|_]) ->
    put(count, 0),
    read_matrix(Filename),
    print_puzzle(),
    case solve() of
        true -> ok;
        false -> io:format("No solution found~n")
    end;
main(_) ->
    io:format(standard_error, "Usage: escript sudoku.erl <matrix_file>~n", []),
    halt(1).

%% Read matrix from file
read_matrix(Filename) ->
    %% Print filename (normalize /app/Matrices to ../Matrices)
    case string:prefix(Filename, "/app/Matrices/") of
        nomatch -> io:format("~s~n", [Filename]);
        Rest -> io:format("../Matrices/~s~n", [Rest])
    end,
    {ok, Binary} = file:read_file(Filename),
    Lines = string:split(binary_to_list(Binary), "\n", all),
    read_rows(Lines, 0).

read_rows(_, 9) -> ok;
read_rows([], _) -> ok;
read_rows([Line|Rest], Row) ->
    Trimmed = string:trim(Line),
    case Trimmed of
        "" -> read_rows(Rest, Row);
        [$#|_] -> read_rows(Rest, Row);  %% Skip comments
        _ ->
            Numbers = string:tokens(Trimmed, " "),
            parse_row(Numbers, Row, 0),
            io:format("~n"),
            read_rows(Rest, Row + 1)
    end.

parse_row(_, _, 9) -> ok;
parse_row([], _, _) -> ok;
parse_row([NumStr|Rest], Row, Col) ->
    Num = list_to_integer(NumStr),
    put({puzzle, Row, Col}, Num),
    io:format("~B ", [Num]),
    parse_row(Rest, Row, Col + 1).

%% Print puzzle in C format
print_puzzle() ->
    io:format("~nPuzzle:~n"),
    lists:foreach(fun(R) ->
        lists:foreach(fun(C) ->
            io:format("~B ", [get({puzzle, R, C})])
        end, lists:seq(0, 8)),
        io:format("~n")
    end, lists:seq(0, 8)).

%% Check if value is valid at position
is_valid(Row, Col, Val) ->
    %% Check row
    RowOk = not lists:any(fun(C) -> get({puzzle, Row, C}) =:= Val end, lists:seq(0, 8)),
    %% Check column
    ColOk = not lists:any(fun(R) -> get({puzzle, R, Col}) =:= Val end, lists:seq(0, 8)),
    %% Check 3x3 box
    BoxRow = (Row div 3) * 3,
    BoxCol = (Col div 3) * 3,
    BoxOk = not lists:any(fun(R) ->
        lists:any(fun(C) -> get({puzzle, R, C}) =:= Val end,
                  lists:seq(BoxCol, BoxCol + 2))
    end, lists:seq(BoxRow, BoxRow + 2)),
    RowOk andalso ColOk andalso BoxOk.

%% Find first empty cell (row-major order)
find_empty() ->
    find_empty(0, 0).

find_empty(9, _) -> none;
find_empty(Row, 9) -> find_empty(Row + 1, 0);
find_empty(Row, Col) ->
    case get({puzzle, Row, Col}) of
        0 -> {Row, Col};
        _ -> find_empty(Row, Col + 1)
    end.

%% Solve the puzzle
solve() ->
    case find_empty() of
        none ->
            %% Puzzle solved
            print_puzzle(),
            io:format("~nSolved in Iterations=~B~n~n", [get(count)]),
            true;
        {Row, Col} ->
            try_values(Row, Col, 1)
    end.

try_values(_, _, 10) -> false;
try_values(Row, Col, Val) ->
    put(count, get(count) + 1),  %% Count EVERY attempt
    case is_valid(Row, Col, Val) of
        true ->
            put({puzzle, Row, Col}, Val),
            case solve() of
                true -> true;
                false ->
                    put({puzzle, Row, Col}, 0),  %% Backtrack
                    try_values(Row, Col, Val + 1)
            end;
        false ->
            try_values(Row, Col, Val + 1)
    end.
