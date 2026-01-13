#!/usr/bin/env escript
%% -*- erlang -*-
%% Erlang Dancing Links (DLX) Sudoku Solver
%% Port of C DLX implementation using ETS table for mutable state

main([Filename|_]) ->
    %% Initialize iteration counter
    ets:new(state, [set, named_table, public]),
    ets:insert(state, {dlx_iterations, 0}),
    ets:insert(state, {node_count, 0}),

    %% Read and print puzzle
    Puzzle = read_matrix(Filename),
    print_puzzle("Puzzle", Puzzle),

    %% Solve using DLX
    case solve_dlx(Puzzle) of
        {ok, Solution} ->
            print_puzzle("Puzzle", Solution),
            [{dlx_iterations, Iters}] = ets:lookup(state, dlx_iterations),
            io:format("~nSolved in Iterations=~B~n~n", [Iters]);
        error ->
            io:format("~nNo solution found~n")
    end,

    ets:delete(state);
main(_) ->
    io:format(standard_error, "Usage: escript dlx.erl <matrix_file>~n", []),
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
    Grid = array:new([{size, 9}, {default, array:new([{size, 9}, {default, 0}])}]),
    read_rows(Lines, 0, Grid).

read_rows(_, 9, Grid) -> Grid;
read_rows([], _, Grid) -> Grid;
read_rows([Line|Rest], Row, Grid) ->
    Trimmed = string:trim(Line),
    case Trimmed of
        "" -> read_rows(Rest, Row, Grid);
        [$#|_] -> read_rows(Rest, Row, Grid);  %% Skip comments
        _ ->
            Numbers = string:tokens(Trimmed, " "),
            NewGrid = parse_row(Numbers, Row, 0, Grid),
            io:format("~n"),
            read_rows(Rest, Row + 1, NewGrid)
    end.

parse_row([], _, _, Grid) -> Grid;
parse_row([NumStr|Rest], Row, Col, Grid) when Col < 9 ->
    Num = list_to_integer(NumStr),
    io:format("~B ", [Num]),
    RowArray = array:get(Row, Grid),
    NewRowArray = array:set(Col, Num, RowArray),
    NewGrid = array:set(Row, NewRowArray, Grid),
    parse_row(Rest, Row, Col + 1, NewGrid);
parse_row(_, _, _, Grid) -> Grid.

%% Print puzzle
print_puzzle(Label, Grid) ->
    io:format("~n~s:~n", [Label]),
    array:map(fun(_R, RowArray) ->
        array:map(fun(_C, Val) ->
            io:format("~B ", [Val]),
            Val
        end, RowArray),
        io:format("~n"),
        RowArray
    end, Grid),
    Grid.

%% Node structure stored in ETS
%% Each node is {NodeId, {left, right, up, down, column, size, row_id, col_id}}

get_node_field(NodeId, Field) ->
    case ets:lookup(state, NodeId) of
        [{NodeId, NodeData}] -> element(Field, NodeData);
        [] -> undefined
    end.

set_node_field(NodeId, Field, Value) ->
    case ets:lookup(state, NodeId) of
        [{NodeId, NodeData}] ->
            NewData = setelement(Field, NodeData, Value),
            ets:insert(state, {NodeId, NewData});
        [] -> ok
    end.

get_node(NodeId) ->
    case ets:lookup(state, NodeId) of
        [{NodeId, NodeData}] -> NodeData;
        [] -> undefined
    end.

set_node(NodeId, NodeData) ->
    ets:insert(state, {NodeId, NodeData}).

%% NodeData = {Left, Right, Up, Down, Column, Size, RowId, ColId}
make_node(NodeId, RowId, ColId) ->
    {NodeId, NodeId, NodeId, NodeId, NodeId, 0, RowId, ColId}.

%% Calculate constraint column indices
get_position_col(R, C) -> {col, R * 9 + C}.
get_row_col(R, N) -> {col, 81 + R * 9 + (N - 1)}.
get_col_col(C, N) -> {col, 162 + C * 9 + (N - 1)}.
get_box_col(R, C, N) ->
    Box = (R div 3) * 3 + (C div 3),
    {col, 243 + Box * 9 + (N - 1)}.

%% Initialize DLX matrix
init_dlx_matrix() ->
    %% Create root column
    set_node(root, make_node(root, -1, -1)),

    %% Create 324 column headers
    lists:foreach(fun(I) ->
        ColId = {col, I},
        set_node(ColId, make_node(ColId, -1, I)),

        %% Link into header list
        {LeftId, RightId, UpId, DownId, ColId2, Size, RowId, ColId3} = get_node(root),
        set_node_field(ColId, 1, LeftId),  %% left
        set_node_field(ColId, 2, root),    %% right
        set_node_field(LeftId, 2, ColId),
        set_node_field(root, 1, ColId)
    end, lists:seq(0, 323)).

%% Add a node to the DLX matrix
add_node(ColId, RowId) ->
    [{node_count, Count}] = ets:lookup(state, node_count),
    NodeId = {node, Count},
    ets:insert(state, {node_count, Count + 1}),

    %% Create node
    set_node(NodeId, make_node(NodeId, RowId, -1)),
    set_node_field(NodeId, 5, ColId),  %% column

    %% Insert at end of column's circular list
    {_,_, UpId, _, _, _, _, _} = get_node(ColId),
    set_node_field(NodeId, 4, ColId),      %% down = column
    set_node_field(NodeId, 3, UpId),       %% up = column.up
    set_node_field(UpId, 4, NodeId),       %% column.up.down = node
    set_node_field(ColId, 3, NodeId),      %% column.up = node

    %% Increment column size
    {_, _, _, _, _, Size, _, _} = get_node(ColId),
    set_node_field(ColId, 6, Size + 1),

    NodeId.

%% Build a DLX row
build_dlx_row(R, C, N, RowId) ->
    %% Store row metadata
    ets:insert(state, {{row_info, RowId}, {R, C, N}}),

    %% Create nodes for the 4 constraints
    N1 = add_node(get_position_col(R, C), RowId),
    N2 = add_node(get_row_col(R, N), RowId),
    N3 = add_node(get_col_col(C, N), RowId),
    N4 = add_node(get_box_col(R, C, N), RowId),

    %% Link nodes horizontally in circular list
    set_node_field(N1, 2, N2),  %% right
    set_node_field(N2, 2, N3),
    set_node_field(N3, 2, N4),
    set_node_field(N4, 2, N1),

    set_node_field(N1, 1, N4),  %% left
    set_node_field(N2, 1, N1),
    set_node_field(N3, 1, N2),
    set_node_field(N4, 1, N3),

    %% Store first node for this row
    ets:insert(state, {{row_start, RowId}, N1}),
    N1.

%% Build matrix from puzzle
build_dlx_matrix(Puzzle) ->
    init_dlx_matrix(),
    build_matrix_rows(Puzzle, 0, 0, 0).

build_matrix_rows(_, 9, _, _) -> ok;
build_matrix_rows(Puzzle, R, 9, RowId) ->
    build_matrix_rows(Puzzle, R + 1, 0, RowId);
build_matrix_rows(Puzzle, R, C, RowId) ->
    RowArray = array:get(R, Puzzle),
    Val = array:get(C, RowArray),
    NewRowId = if
        Val =/= 0 ->
            build_dlx_row(R, C, Val, RowId),
            RowId + 1;
        true ->
            lists:foldl(fun(N, AccRowId) ->
                build_dlx_row(R, C, N, AccRowId),
                AccRowId + 1
            end, RowId, lists:seq(1, 9))
    end,
    build_matrix_rows(Puzzle, R, C + 1, NewRowId).

%% Cover a column
cover_column(ColId) ->
    %% Remove column header from header list
    {LeftId, RightId, _, _, _, _, _, _} = get_node(ColId),
    set_node_field(RightId, 1, LeftId),
    set_node_field(LeftId, 2, RightId),

    %% For each row in this column
    {_, _, _, DownId, _, _, _, _} = get_node(ColId),
    cover_rows(DownId, ColId).

cover_rows(RowNode, ColId) when RowNode =:= ColId -> ok;
cover_rows(RowNode, ColId) ->
    %% For each node in this row
    {_, RightId, _, _, _, _, _, _} = get_node(RowNode),
    cover_row_nodes(RightId, RowNode),

    {_, _, _, DownId, _, _, _, _} = get_node(RowNode),
    cover_rows(DownId, ColId).

cover_row_nodes(RightNode, RowNode) when RightNode =:= RowNode -> ok;
cover_row_nodes(RightNode, RowNode) ->
    %% Remove this node from its column
    {_, _, UpId, DownId, Column, _, _, _} = get_node(RightNode),
    set_node_field(DownId, 3, UpId),
    set_node_field(UpId, 4, DownId),

    {_, _, _, _, _, Size, _, _} = get_node(Column),
    set_node_field(Column, 6, Size - 1),

    {_, RightId2, _, _, _, _, _, _} = get_node(RightNode),
    cover_row_nodes(RightId2, RowNode).

%% Uncover a column
uncover_column(ColId) ->
    %% For each row in this column (in reverse order)
    {_, _, UpId, _, _, _, _, _} = get_node(ColId),
    uncover_rows(UpId, ColId),

    %% Restore column header to header list
    {LeftId, RightId, _, _, _, _, _, _} = get_node(ColId),
    set_node_field(RightId, 1, ColId),
    set_node_field(LeftId, 2, ColId).

uncover_rows(RowNode, ColId) when RowNode =:= ColId -> ok;
uncover_rows(RowNode, ColId) ->
    %% For each node in this row (in reverse order)
    {LeftId, _, _, _, _, _, _, _} = get_node(RowNode),
    uncover_row_nodes(LeftId, RowNode),

    {_, _, UpId, _, _, _, _, _} = get_node(RowNode),
    uncover_rows(UpId, ColId).

uncover_row_nodes(LeftNode, RowNode) when LeftNode =:= RowNode -> ok;
uncover_row_nodes(LeftNode, RowNode) ->
    %% Restore this node to its column
    {_, _, UpId, DownId, Column, _, _, _} = get_node(LeftNode),
    {_, _, _, _, _, Size, _, _} = get_node(Column),
    set_node_field(Column, 6, Size + 1),
    set_node_field(DownId, 3, LeftNode),
    set_node_field(UpId, 4, LeftNode),

    {LeftId2, _, _, _, _, _, _, _} = get_node(LeftNode),
    uncover_row_nodes(LeftId2, RowNode).

%% Choose column with minimum size
choose_column(Root) ->
    {_, RightId, _, _, _, _, _, _} = get_node(Root),
    choose_column_scan(RightId, Root, none, 9999999).

choose_column_scan(ColNode, Root, Best, _MinSize) when ColNode =:= Root ->
    Best;
choose_column_scan(ColNode, Root, none, _MinSize) ->
    {_, RightId, _, _, _, Size, _, _} = get_node(ColNode),
    choose_column_scan(RightId, Root, ColNode, Size);
choose_column_scan(ColNode, Root, Best, MinSize) ->
    {_, RightId, _, _, _, Size, _, _} = get_node(ColNode),
    {NewBest, NewMinSize} = if
        Size < MinSize -> {ColNode, Size};
        true -> {Best, MinSize}
    end,
    choose_column_scan(RightId, Root, NewBest, NewMinSize).

%% DLX Search
dlx_search(Root, K, Solution) ->
    %% Increment iteration counter
    [{dlx_iterations, Iters}] = ets:lookup(state, dlx_iterations),
    ets:insert(state, {dlx_iterations, Iters + 1}),

    %% If matrix is empty, we found a solution
    {_, RightId, _, _, _, _, _, _} = get_node(Root),
    if
        RightId =:= Root -> {ok, Solution};
        true ->
            %% Choose column with minimum size
            case choose_column(Root) of
                none -> error;
                Col ->
                    {_, _, _, _, _, Size, _, _} = get_node(Col),
                    if
                        Size =:= 0 -> error;
                        true ->
                            cover_column(Col),
                            {_, _, _, DownId, _, _, _, _} = get_node(Col),
                            try_rows(DownId, Col, Root, K, Solution)
                    end
            end
    end.

try_rows(RowNode, Col, _Root, _K, _Solution) when RowNode =:= Col ->
    uncover_column(Col),
    error;
try_rows(RowNode, Col, Root, K, Solution) ->
    %% Add row to partial solution
    {_, _, _, _, _, _, RowId, _} = get_node(RowNode),
    NewSolution = array:set(K, RowId, Solution),

    %% Cover all other columns in this row
    {_, RightId, _, _, _, _, _, _} = get_node(RowNode),
    cover_row_cols(RightId, RowNode),

    %% Recurse
    case dlx_search(Root, K + 1, NewSolution) of
        {ok, FinalSolution} -> {ok, FinalSolution};
        error ->
            %% Backtrack
            {LeftId, _, _, _, _, _, _, _} = get_node(RowNode),
            uncover_row_cols(LeftId, RowNode),

            {_, _, _, DownId, _, _, _, _} = get_node(RowNode),
            try_rows(DownId, Col, Root, K, Solution)
    end.

cover_row_cols(RightNode, RowNode) when RightNode =:= RowNode -> ok;
cover_row_cols(RightNode, RowNode) ->
    {_, _, _, _, Column, _, _, _} = get_node(RightNode),
    cover_column(Column),
    {_, RightId, _, _, _, _, _, _} = get_node(RightNode),
    cover_row_cols(RightId, RowNode).

uncover_row_cols(LeftNode, RowNode) when LeftNode =:= RowNode -> ok;
uncover_row_cols(LeftNode, RowNode) ->
    {_, _, _, _, Column, _, _, _} = get_node(LeftNode),
    uncover_column(Column),
    {LeftId, _, _, _, _, _, _, _} = get_node(LeftNode),
    uncover_row_cols(LeftId, RowNode).

%% Cover given clues
cover_clues(Puzzle) ->
    cover_clues_iter(Puzzle, 0, 0).

cover_clues_iter(_, 9, _) -> ok;
cover_clues_iter(Puzzle, R, 9) ->
    cover_clues_iter(Puzzle, R + 1, 0);
cover_clues_iter(Puzzle, R, C) ->
    RowArray = array:get(R, Puzzle),
    Val = array:get(C, RowArray),
    if
        Val =/= 0 ->
            %% Find the row for this clue by scanning all rows
            find_and_cover_clue_row(R, C, Val);
        true -> ok
    end,
    cover_clues_iter(Puzzle, R, C + 1).

find_and_cover_clue_row(R, C, N) ->
    %% Scan all row_info entries to find the matching row
    AllKeys = ets:match(state, {{row_info, '$1'}, '_'}),
    find_clue_in_rows(lists:flatten(AllKeys), R, C, N).

find_clue_in_rows([], _, _, _) -> ok;
find_clue_in_rows([RowId|Rest], R, C, N) ->
    case ets:lookup(state, {row_info, RowId}) of
        [{{row_info, RowId}, {R, C, N}}] ->
            [{{row_start, RowId}, StartNode}] = ets:lookup(state, {row_start, RowId}),
            cover_clue_cols(StartNode, StartNode);
        _ ->
            find_clue_in_rows(Rest, R, C, N)
    end.

cover_clue_cols(Curr, StartNode) ->
    {_, _, _, _, Column, _, _, _} = get_node(Curr),
    cover_column(Column),
    {_, RightId, _, _, _, _, _, _} = get_node(Curr),
    if
        RightId =:= StartNode -> ok;
        true -> cover_clue_cols(RightId, StartNode)
    end.

%% Solve DLX
solve_dlx(Puzzle) ->
    build_dlx_matrix(Puzzle),
    cover_clues(Puzzle),

    Solution = array:new([{size, 81}, {default, -1}]),
    case dlx_search(root, 0, Solution) of
        {ok, Sol} -> {ok, extract_solution(Puzzle, Sol)};
        error -> error
    end.

%% Extract solution
extract_solution(Puzzle, Solution) ->
    %% Start with original puzzle
    FinalGrid = array:map(fun(R, RowArray) ->
        array:map(fun(C, Val) -> Val end, RowArray)
    end, Puzzle),

    %% Apply solution
    extract_solution_rows(0, Solution, FinalGrid).

extract_solution_rows(81, _, Grid) -> Grid;
extract_solution_rows(I, Solution, Grid) ->
    case array:get(I, Solution) of
        -1 -> extract_solution_rows(I + 1, Solution, Grid);
        RowId ->
            case ets:lookup(state, {row_info, RowId}) of
                [{{row_info, RowId}, {R, C, Num}}] ->
                    RowArray = array:get(R, Grid),
                    NewRowArray = array:set(C, Num, RowArray),
                    NewGrid = array:set(R, NewRowArray, Grid),
                    extract_solution_rows(I + 1, Solution, NewGrid);
                [] ->
                    extract_solution_rows(I + 1, Solution, Grid)
            end
    end.
