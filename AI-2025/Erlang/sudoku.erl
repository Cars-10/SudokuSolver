-module(sudoku).


-export([start/0]).

start() ->
    Args = init:get_plain_arguments(),
    Start = os:system_time(millisecond),
    process_args(Args),
    End = os:system_time(millisecond),
    io:format("Seconds to process ~.3f~n", [(End - Start) / 1000.0]).

process_args([]) -> ok;
process_args([Filename | Rest]) ->
    case string:find(Filename, ".matrix") of
        nomatch -> process_args(Rest);
        _ ->
            process_file(Filename),
            process_args(Rest)
    end.

process_file(Filename) ->
    io:format("~s~n", [Filename]),
    {ok, Binary} = file:read_file(Filename),
    Lines = binary:split(Binary, <<"\n">>, [global]),
    Board = parse_board(Lines, []),
    print_board(Board),
    {SolvedBoard, Iterations} = solve(Board),
    print_board(SolvedBoard),
    io:format("Solved in Iterations=~p~n~n", [Iterations]).

parse_board([], Acc) -> lists:reverse(Acc);
parse_board([Line | Rest], Acc) ->
    CleanLine = string:trim(Line),
    case CleanLine of
        <<>> -> parse_board(Rest, Acc);
        <<"#", _/binary>> -> parse_board(Rest, Acc);
        _ ->
            Parts = binary:split(CleanLine, <<" ">>, [global, trim_all]),
            if length(Parts) == 9 ->
                Row = [binary_to_integer(X) || X <- Parts],
                parse_board(Rest, [Row | Acc]);
            true -> parse_board(Rest, Acc)
            end
    end.

print_board(Board) ->
    io:format("~nPuzzle:~n"),
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Val) -> io:format("~p ", [Val]) end, Row),
        io:format("~n")
    end, Board).

solve(Board) ->
    solve(Board, 0).

solve(Board, Count) ->
    case find_empty(Board) of
        none -> {Board, Count};
        {Row, Col} ->
            try_values(Board, Row, Col, 1, Count)
    end.

try_values(_Board, _Row, _Col, 10, Count) -> {none, Count};
try_values(Board, Row, Col, Val, Count) ->
    NewCount = Count + 1,
    case is_possible(Board, Row, Col, Val) of
        true ->
            NewBoard = set_val(Board, Row, Col, Val),
            case solve(NewBoard, NewCount) of
                {none, FinalCount} -> try_values(Board, Row, Col, Val + 1, FinalCount);
                {SolvedBoard, FinalCount} -> {SolvedBoard, FinalCount}
            end;
        false ->
            try_values(Board, Row, Col, Val + 1, NewCount)
    end.

find_empty(Board) ->
    find_empty(Board, 0).

find_empty([], _R) -> none;
find_empty([Row | Rest], R) ->
    case find_empty_col(Row, 0) of
        none -> find_empty(Rest, R + 1);
        C -> {R, C}
    end.

find_empty_col([], _C) -> none;
find_empty_col([0 | _], C) -> C;
find_empty_col([_ | Rest], C) -> find_empty_col(Rest, C + 1).

get_val(Board, R, C) ->
    lists:nth(C + 1, lists:nth(R + 1, Board)).

set_val(Board, R, C, Val) ->
    {RowList, [Row | RestRows]} = lists:split(R, Board),
    {ColList, [_ | RestCols]} = lists:split(C, Row),
    NewRow = ColList ++ [Val] ++ RestCols,
    RowList ++ [NewRow] ++ RestRows.

is_possible(Board, R, C, Val) ->
    not (in_row(Board, R, Val) or in_col(Board, C, Val) or in_box(Board, R, C, Val)).

in_row(Board, R, Val) ->
    lists:member(Val, lists:nth(R + 1, Board)).

in_col(Board, C, Val) ->
    ColVals = [lists:nth(C + 1, Row) || Row <- Board],
    lists:member(Val, ColVals).

in_box(Board, R, C, Val) ->
    R0 = (R div 3) * 3,
    C0 = (C div 3) * 3,
    BoxVals = [get_val(Board, Ri, Ci) || Ri <- lists:seq(R0, R0 + 2), Ci <- lists:seq(C0, C0 + 2)],
    lists:member(Val, BoxVals).
