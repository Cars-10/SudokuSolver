-module(sudoku).
-export([main/1]).

main(Args) ->
    lists:foreach(fun process_file/1, Args).

process_file(Filename) ->
    io:format("~nProcessing ~s~n", [Filename]),
    case read_board(Filename) of
        {ok, Board} ->
            print_board(Board),
            put(iterations, 0),
            case solve(Board, 0, 0) of
                {ok, SolvedBoard} ->
                    print_board(SolvedBoard),
                    io:format("~nSolved in Iterations=~p~n", [get(iterations)]);
                error ->
                    io:format("No solution found~n")
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [Filename, Reason])
    end.

read_board(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Lines = binary:split(Binary, <<"\n">>, [global, trim]),
            FilteredLines = lists:filter(fun(Line) -> 
                case binary:match(Line, <<"#">>) of
                    {0, _} -> false;
                    _ -> byte_size(Line) > 0
                end
            end, Lines),
            Rows = lists:sublist(FilteredLines, 9),
            Board = lists:map(fun(Line) ->
                Parts = binary:split(Line, <<" ">>, [global, trim_all]),
                lists:map(fun(Part) -> binary_to_integer(Part) end, lists:sublist(Parts, 9))
            end, Rows),
            {ok, list_to_tuple(lists:map(fun list_to_tuple/1, Board))};
        Error -> Error
    end.

print_board(Board) ->
    io:format("Puzzle:~n"),
    lists:foreach(fun(RowIndex) ->
        Row = element(RowIndex, Board),
        lists:foreach(fun(ColIndex) ->
            io:format("~p ", [element(ColIndex, Row)])
        end, lists:seq(1, 9)),
        io:format("~n")
    end, lists:seq(1, 9)).

is_possible(Board, Row, Col, Num) ->
    RowData = element(Row + 1, Board),
    RowValid = not lists:member(Num, tuple_to_list(RowData)),
    ColValid = not lists:any(fun(R) -> element(Col + 1, element(R, Board)) == Num end, lists:seq(1, 9)),
    
    StartRow = (Row div 3) * 3,
    StartCol = (Col div 3) * 3,
    BoxValid = not lists:any(fun(R) ->
        lists:any(fun(C) ->
            element(C + 1, element(R + 1, Board)) == Num
        end, lists:seq(StartCol, StartCol + 2))
    end, lists:seq(StartRow, StartRow + 2)),
    
    RowValid andalso ColValid andalso BoxValid.

solve(Board, 9, _) -> {ok, Board};
solve(Board, Row, 9) -> solve(Board, Row + 1, 0);
solve(Board, Row, Col) ->
    CurrentVal = element(Col + 1, element(Row + 1, Board)),
    if
        CurrentVal /= 0 ->
            solve(Board, Row, Col + 1);
        true ->
            try_numbers(Board, Row, Col, 1)
    end.

try_numbers(_Board, _Row, _Col, 10) -> error;
try_numbers(Board, Row, Col, Num) ->
    put(iterations, get(iterations) + 1),
    case is_possible(Board, Row, Col, Num) of
        true ->
            NewRowData = setelement(Col + 1, element(Row + 1, Board), Num),
            NewBoard = setelement(Row + 1, Board, NewRowData),
            case solve(NewBoard, Row, Col + 1) of
                {ok, SolvedBoard} -> {ok, SolvedBoard};
                error -> try_numbers(Board, Row, Col, Num + 1)
            end;
        false ->
            try_numbers(Board, Row, Col, Num + 1)
    end.
