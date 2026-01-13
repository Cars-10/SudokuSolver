#!/usr/bin/env escript
%% -*- erlang -*-
%% Erlang Constraint Propagation (CP) Sudoku Solver
%% Port of C CP implementation using arrays and bitsets

main([Filename|_]) ->
    %% Initialize iteration counter using process dictionary
    put(cp_iterations, 0),

    %% Read puzzle
    Puzzle = read_matrix(Filename),
    print_puzzle("Puzzle", Puzzle),

    %% Initialize grid
    {Grid, Candidates} = init_grid(Puzzle),

    %% Solve using CP
    case solve_cp(Grid, Candidates) of
        {ok, Solution} ->
            print_puzzle("Puzzle", Solution),
            io:format("~nSolved in Iterations=~B~n~n", [get(cp_iterations)]);
        error ->
            io:format("~nNo solution found~n")
    end;
main(_) ->
    io:format(standard_error, "Usage: escript cp.erl <matrix_file>~n", []),
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

%% Bitset helpers
has_candidate(Set, Digit) -> (Set band (1 bsl Digit)) =/= 0.

remove_candidate(Set, Digit) -> Set band (bnot (1 bsl Digit)).

%% Count number of set bits (popcount)
count_candidates(Set) ->
    count_bits(Set, 0).

count_bits(0, Acc) -> Acc;
count_bits(N, Acc) -> count_bits(N band (N - 1), Acc + 1).

%% Get first candidate digit from bitset (1-9)
get_first_candidate(Cs) ->
    get_first_candidate(Cs, 1).

get_first_candidate(_, 10) -> 0;
get_first_candidate(Cs, Digit) ->
    case has_candidate(Cs, Digit) of
        true -> Digit;
        false -> get_first_candidate(Cs, Digit + 1)
    end.

%% Get all 20 peers for a cell (row, col, box)
get_peers(Row, Col) ->
    %% Same row (8 cells)
    RowPeers = [{Row, C} || C <- lists:seq(0, 8), C =/= Col],

    %% Same column (8 cells)
    ColPeers = [{R, Col} || R <- lists:seq(0, 8), R =/= Row],

    %% Same 3x3 box (4 cells not already counted)
    BoxRow = (Row div 3) * 3,
    BoxCol = (Col div 3) * 3,
    BoxPeers = [{R, C} || R <- lists:seq(BoxRow, BoxRow + 2),
                          C <- lists:seq(BoxCol, BoxCol + 2),
                          R =/= Row andalso C =/= Col],

    RowPeers ++ ColPeers ++ BoxPeers.

%% Initialize grid from puzzle
init_grid(Puzzle) ->
    Grid = array:new([{size, 9}, {default, array:new([{size, 9}, {default, 0}])}]),
    Candidates = array:new([{size, 9}, {default, array:new([{size, 9}, {default, 0}])}]),

    {NewGrid, NewCandidates} = init_cells(Puzzle, Grid, Candidates, 0, 0),

    %% Initial constraint propagation for given clues
    {NewGrid, NewCandidates}.

init_cells(_, Grid, Candidates, 9, _) -> {Grid, Candidates};
init_cells(Puzzle, Grid, Candidates, Row, 9) ->
    init_cells(Puzzle, Grid, Candidates, Row + 1, 0);
init_cells(Puzzle, Grid, Candidates, Row, Col) ->
    PuzzleRow = array:get(Row, Puzzle),
    Val = array:get(Col, PuzzleRow),

    {NewGrid, NewCandidates} = if
        Val =:= 0 ->
            %% Empty cell: set all candidates 1-9
            GridRow = array:get(Row, Grid),
            NewGridRow = array:set(Col, 0, GridRow),
            G = array:set(Row, NewGridRow, Grid),

            CandRow = array:get(Row, Candidates),
            NewCandRow = array:set(Col, 16#3FE, CandRow),  %% Binary: 0011 1111 1110 (bits 1-9)
            C = array:set(Row, NewCandRow, Candidates),
            {G, C};
        true ->
            %% Given clue: set single value
            GridRow = array:get(Row, Grid),
            NewGridRow = array:set(Col, Val, GridRow),
            G = array:set(Row, NewGridRow, Grid),

            CandRow = array:get(Row, Candidates),
            NewCandRow = array:set(Col, 1 bsl Val, CandRow),
            C = array:set(Row, NewCandRow, Candidates),
            {G, C}
    end,

    init_cells(Puzzle, NewGrid, NewCandidates, Row, Col + 1).

%% Get grid value
get_grid(Grid, Row, Col) ->
    RowArray = array:get(Row, Grid),
    array:get(Col, RowArray).

%% Set grid value
set_grid(Grid, Row, Col, Val) ->
    RowArray = array:get(Row, Grid),
    NewRowArray = array:set(Col, Val, RowArray),
    array:set(Row, NewRowArray, Grid).

%% Get candidates
get_candidates(Candidates, Row, Col) ->
    RowArray = array:get(Row, Candidates),
    array:get(Col, RowArray).

%% Set candidates
set_candidates(Candidates, Row, Col, Val) ->
    RowArray = array:get(Row, Candidates),
    NewRowArray = array:set(Col, Val, RowArray),
    array:set(Row, NewRowArray, Candidates).

%% Eliminate a digit from a cell's candidates
eliminate(Grid, Candidates, Row, Col, Digit) ->
    CandSet = get_candidates(Candidates, Row, Col),

    %% Check if digit is already eliminated
    case has_candidate(CandSet, Digit) of
        false ->
            {ok, Grid, Candidates};  %% Already eliminated
        true ->
            %% Remove digit from candidates
            NewCandSet = remove_candidate(CandSet, Digit),
            NewCandidates = set_candidates(Candidates, Row, Col, NewCandSet),

            %% Check for contradiction
            Remaining = count_candidates(NewCandSet),
            if
                Remaining =:= 0 ->
                    error;  %% Contradiction
                Remaining =:= 1 ->
                    case get_grid(Grid, Row, Col) of
                        0 ->
                            %% Singleton elimination
                            LastDigit = get_first_candidate(NewCandSet),
                            assign(Grid, NewCandidates, Row, Col, LastDigit);
                        _ ->
                            {ok, Grid, NewCandidates}
                    end;
                true ->
                    {ok, Grid, NewCandidates}
            end
    end.

%% Assign a digit to a cell
assign(Grid, Candidates, Row, Col, Digit) ->
    %% Increment iteration counter
    put(cp_iterations, get(cp_iterations) + 1),

    %% Set value
    NewGrid = set_grid(Grid, Row, Col, Digit),
    NewCandidates = set_candidates(Candidates, Row, Col, 1 bsl Digit),

    %% Eliminate digit from all peers
    Peers = get_peers(Row, Col),
    eliminate_from_peers(NewGrid, NewCandidates, Peers, Digit).

eliminate_from_peers(Grid, Candidates, [], _Digit) ->
    {ok, Grid, Candidates};
eliminate_from_peers(Grid, Candidates, [{PeerRow, PeerCol}|Rest], Digit) ->
    case eliminate(Grid, Candidates, PeerRow, PeerCol, Digit) of
        {ok, NewGrid, NewCandidates} ->
            eliminate_from_peers(NewGrid, NewCandidates, Rest, Digit);
        error -> error
    end.

%% Constraint propagation
propagate(Grid, Candidates) ->
    case propagate_loop(Grid, Candidates) of
        {ok, NewGrid, NewCandidates, _Changed} -> {ok, NewGrid, NewCandidates};
        error -> error
    end.

propagate_loop(Grid, Candidates) ->
    %% Strategy 1: Singleton elimination
    case singleton_elimination(Grid, Candidates, 0, 0) of
        {ok, NewGrid, NewCandidates, Changed1} ->
            %% Strategy 2: Hidden singles - rows
            case hidden_singles_rows(NewGrid, NewCandidates, 0, 1) of
                {ok, NewGrid2, NewCandidates2, Changed2} ->
                    %% Strategy 3: Hidden singles - columns
                    case hidden_singles_cols(NewGrid2, NewCandidates2, 0, 1) of
                        {ok, NewGrid3, NewCandidates3, Changed3} ->
                            %% Strategy 4: Hidden singles - boxes
                            case hidden_singles_boxes(NewGrid3, NewCandidates3, 0, 1) of
                                {ok, NewGrid4, NewCandidates4, Changed4} ->
                                    Changed = Changed1 orelse Changed2 orelse Changed3 orelse Changed4,
                                    if
                                        Changed -> propagate_loop(NewGrid4, NewCandidates4);
                                        true -> {ok, NewGrid4, NewCandidates4, false}
                                    end;
                                error -> error
                            end;
                        error -> error
                    end;
                error -> error
            end;
        error -> error
    end.

%% Singleton elimination
singleton_elimination(Grid, Candidates, 9, _) -> {ok, Grid, Candidates, false};
singleton_elimination(Grid, Candidates, Row, 9) ->
    singleton_elimination(Grid, Candidates, Row + 1, 0);
singleton_elimination(Grid, Candidates, Row, Col) ->
    case get_grid(Grid, Row, Col) of
        0 ->
            CandSet = get_candidates(Candidates, Row, Col),
            NumCandidates = count_candidates(CandSet),
            if
                NumCandidates =:= 0 -> error;  %% Contradiction
                NumCandidates =:= 1 ->
                    Digit = get_first_candidate(CandSet),
                    case assign(Grid, Candidates, Row, Col, Digit) of
                        {ok, NewGrid, NewCandidates} ->
                            {ok, NewGrid, NewCandidates, true};  %% Changed
                        error -> error
                    end;
                true ->
                    singleton_elimination(Grid, Candidates, Row, Col + 1)
            end;
        _ ->
            singleton_elimination(Grid, Candidates, Row, Col + 1)
    end.

%% Hidden singles - rows
hidden_singles_rows(Grid, Candidates, 9, _) -> {ok, Grid, Candidates, false};
hidden_singles_rows(Grid, Candidates, Row, 10) ->
    hidden_singles_rows(Grid, Candidates, Row + 1, 1);
hidden_singles_rows(Grid, Candidates, Row, Digit) ->
    {Count, LastCol, AlreadyAssigned} = count_digit_in_row(Grid, Candidates, Row, Digit, 0),
    if
        AlreadyAssigned -> hidden_singles_rows(Grid, Candidates, Row, Digit + 1);
        Count =:= 1 ->
            case assign(Grid, Candidates, Row, LastCol, Digit) of
                {ok, NewGrid, NewCandidates} ->
                    {ok, NewGrid, NewCandidates, true};  %% Changed
                error -> error
            end;
        Count =:= 0 -> error;  %% Contradiction
        true -> hidden_singles_rows(Grid, Candidates, Row, Digit + 1)
    end.

count_digit_in_row(_Grid, _Candidates, _Row, _Digit, Col) when Col =:= 9 ->
    {0, -1, false};
count_digit_in_row(Grid, Candidates, Row, Digit, Col) ->
    case get_grid(Grid, Row, Col) of
        Digit -> {0, -1, true};  %% Already assigned
        0 ->
            CandSet = get_candidates(Candidates, Row, Col),
            HasDig = has_candidate(CandSet, Digit),
            {RestCount, RestLastCol, RestAssigned} = count_digit_in_row(Grid, Candidates, Row, Digit, Col + 1),
            if
                RestAssigned -> {0, -1, true};
                HasDig -> {RestCount + 1, Col, false};
                true -> {RestCount, RestLastCol, false}
            end;
        _ ->
            count_digit_in_row(Grid, Candidates, Row, Digit, Col + 1)
    end.

%% Hidden singles - columns
hidden_singles_cols(Grid, Candidates, 9, _) -> {ok, Grid, Candidates, false};
hidden_singles_cols(Grid, Candidates, Col, 10) ->
    hidden_singles_cols(Grid, Candidates, Col + 1, 1);
hidden_singles_cols(Grid, Candidates, Col, Digit) ->
    {Count, LastRow, AlreadyAssigned} = count_digit_in_col(Grid, Candidates, Col, Digit, 0),
    if
        AlreadyAssigned -> hidden_singles_cols(Grid, Candidates, Col, Digit + 1);
        Count =:= 1 ->
            case assign(Grid, Candidates, LastRow, Col, Digit) of
                {ok, NewGrid, NewCandidates} ->
                    {ok, NewGrid, NewCandidates, true};  %% Changed
                error -> error
            end;
        Count =:= 0 -> error;  %% Contradiction
        true -> hidden_singles_cols(Grid, Candidates, Col, Digit + 1)
    end.

count_digit_in_col(_Grid, _Candidates, _Col, _Digit, Row) when Row =:= 9 ->
    {0, -1, false};
count_digit_in_col(Grid, Candidates, Col, Digit, Row) ->
    case get_grid(Grid, Row, Col) of
        Digit -> {0, -1, true};  %% Already assigned
        0 ->
            CandSet = get_candidates(Candidates, Row, Col),
            HasDig = has_candidate(CandSet, Digit),
            {RestCount, RestLastRow, RestAssigned} = count_digit_in_col(Grid, Candidates, Col, Digit, Row + 1),
            if
                RestAssigned -> {0, -1, true};
                HasDig -> {RestCount + 1, Row, false};
                true -> {RestCount, RestLastRow, false}
            end;
        _ ->
            count_digit_in_col(Grid, Candidates, Col, Digit, Row + 1)
    end.

%% Hidden singles - boxes
hidden_singles_boxes(Grid, Candidates, 9, _) -> {ok, Grid, Candidates, false};
hidden_singles_boxes(Grid, Candidates, BoxIdx, 10) ->
    hidden_singles_boxes(Grid, Candidates, BoxIdx + 1, 1);
hidden_singles_boxes(Grid, Candidates, BoxIdx, Digit) ->
    BoxRow = (BoxIdx div 3) * 3,
    BoxCol = (BoxIdx rem 3) * 3,
    {Count, LastRow, LastCol, AlreadyAssigned} = count_digit_in_box(Grid, Candidates, BoxRow, BoxCol, Digit),
    if
        AlreadyAssigned -> hidden_singles_boxes(Grid, Candidates, BoxIdx, Digit + 1);
        Count =:= 1 ->
            case assign(Grid, Candidates, LastRow, LastCol, Digit) of
                {ok, NewGrid, NewCandidates} ->
                    {ok, NewGrid, NewCandidates, true};  %% Changed
                error -> error
            end;
        Count =:= 0 -> error;  %% Contradiction
        true -> hidden_singles_boxes(Grid, Candidates, BoxIdx, Digit + 1)
    end.

count_digit_in_box(Grid, Candidates, BoxRow, BoxCol, Digit) ->
    Cells = [{R, C} || R <- lists:seq(BoxRow, BoxRow + 2),
                       C <- lists:seq(BoxCol, BoxCol + 2)],
    count_digit_in_cells(Grid, Candidates, Cells, Digit, 0, -1, -1).

count_digit_in_cells(_Grid, _Candidates, [], _Digit, Count, LastR, LastC) ->
    {Count, LastR, LastC, false};
count_digit_in_cells(Grid, Candidates, [{R, C}|Rest], Digit, Count, LastR, LastC) ->
    case get_grid(Grid, R, C) of
        Digit -> {0, -1, -1, true};  %% Already assigned
        0 ->
            CandSet = get_candidates(Candidates, R, C),
            HasDig = has_candidate(CandSet, Digit),
            if
                HasDig -> count_digit_in_cells(Grid, Candidates, Rest, Digit, Count + 1, R, C);
                true -> count_digit_in_cells(Grid, Candidates, Rest, Digit, Count, LastR, LastC)
            end;
        _ ->
            count_digit_in_cells(Grid, Candidates, Rest, Digit, Count, LastR, LastC)
    end.

%% Check if puzzle is solved
is_solved(Grid) ->
    is_solved_check(Grid, 0, 0).

is_solved_check(_, 9, _) -> true;
is_solved_check(Grid, Row, 9) ->
    is_solved_check(Grid, Row + 1, 0);
is_solved_check(Grid, Row, Col) ->
    case get_grid(Grid, Row, Col) of
        0 -> false;
        _ -> is_solved_check(Grid, Row, Col + 1)
    end.

%% Find cell with minimum remaining values (MRV heuristic)
find_mrv_cell(Grid, Candidates) ->
    find_mrv_cell(Grid, Candidates, 0, 0, none, 10).

find_mrv_cell(_, _, 9, _, Best, _) -> Best;
find_mrv_cell(Grid, Candidates, Row, 9, Best, MinCount) ->
    find_mrv_cell(Grid, Candidates, Row + 1, 0, Best, MinCount);
find_mrv_cell(Grid, Candidates, Row, Col, Best, MinCount) ->
    case get_grid(Grid, Row, Col) of
        0 ->
            CandSet = get_candidates(Candidates, Row, Col),
            Count = count_candidates(CandSet),
            if
                Count < MinCount ->
                    find_mrv_cell(Grid, Candidates, Row, Col + 1, {Row, Col}, Count);
                true ->
                    find_mrv_cell(Grid, Candidates, Row, Col + 1, Best, MinCount)
            end;
        _ ->
            find_mrv_cell(Grid, Candidates, Row, Col + 1, Best, MinCount)
    end.

%% Search with backtracking
search(Grid, Candidates) ->
    case is_solved(Grid) of
        true -> {ok, Grid};
        false ->
            %% Propagate constraints
            case propagate(Grid, Candidates) of
                {ok, NewGrid, NewCandidates} ->
                    case is_solved(NewGrid) of
                        true -> {ok, NewGrid};
                        false ->
                            %% Find cell with MRV
                            case find_mrv_cell(NewGrid, NewCandidates) of
                                none -> error;  %% No empty cells but not solved (contradiction)
                                {Row, Col} ->
                                    CandSet = get_candidates(NewCandidates, Row, Col),
                                    try_candidates(NewGrid, NewCandidates, Row, Col, CandSet, 1)
                            end
                    end;
                error -> error
            end
    end.

try_candidates(_Grid, _Candidates, _Row, _Col, _CandSet, 10) -> error;
try_candidates(Grid, Candidates, Row, Col, CandSet, Digit) ->
    case has_candidate(CandSet, Digit) of
        true ->
            %% Try this candidate
            case assign(Grid, Candidates, Row, Col, Digit) of
                {ok, NewGrid, NewCandidates} ->
                    case search(NewGrid, NewCandidates) of
                        {ok, Solution} -> {ok, Solution};
                        error ->
                            %% Backtrack
                            try_candidates(Grid, Candidates, Row, Col, CandSet, Digit + 1)
                    end;
                error ->
                    %% Assignment caused contradiction
                    try_candidates(Grid, Candidates, Row, Col, CandSet, Digit + 1)
            end;
        false ->
            try_candidates(Grid, Candidates, Row, Col, CandSet, Digit + 1)
    end.

%% Solve CP
solve_cp(Grid, Candidates) ->
    search(Grid, Candidates).
