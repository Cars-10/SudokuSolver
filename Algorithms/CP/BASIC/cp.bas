/'
Constraint Propagation (CP) Sudoku Solver - FreeBASIC Implementation
Mechanical translation from Python reference to preserve algorithm correctness.

Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic
- Uses bitsets to track candidate values (bits 1-9)
- Propagates constraints: singleton elimination, hidden singles
- Search with MRV cell selection for efficiency
'/

#include "file.bi"
#include "string.bi"

' Global iteration counter
Dim Shared As Integer cp_iterations
Dim Shared As Integer puzzle(0 To 8, 0 To 8)
Dim Shared As Integer solution(0 To 80)

' Grid structure with assigned values and candidate tracking
Type CPGrid
    values(0 To 8, 0 To 8) As Integer        ' Assigned values (0 = empty)
    candidates(0 To 8, 0 To 8) As Integer    ' Possible values per cell (bitset)
End Type

' Peer list structure
Type PeerList
    count As Integer
    rows(0 To 19) As Integer
    cols(0 To 19) As Integer
End Type

' ============================================================================
' Bitset operations
' ============================================================================

Function has_candidate(candidate_set As Integer, digit As Integer) As Integer
    Return (candidate_set And (1 Shl digit)) <> 0
End Function

Function add_candidate(candidate_set As Integer, digit As Integer) As Integer
    Return candidate_set Or (1 Shl digit)
End Function

Function remove_candidate(candidate_set As Integer, digit As Integer) As Integer
    Return candidate_set And (Not (1 Shl digit))
End Function

Function count_candidates(candidate_set As Integer) As Integer
    Dim As Integer count = 0
    Dim As Integer temp = candidate_set
    While temp <> 0
        count += (temp And 1)
        temp = temp Shr 1
    Wend
    Return count
End Function

Function get_first_candidate(candidate_set As Integer) As Integer
    For digit As Integer = 1 To 9
        If has_candidate(candidate_set, digit) Then
            Return digit
        End If
    Next
    Return 0
End Function

' ============================================================================
' Grid operations
' ============================================================================

Sub get_peers(row As Integer, col As Integer, peers As PeerList Ptr)
    peers->count = 0

    ' Same row (8 cells)
    For c As Integer = 0 To 8
        If c <> col Then
            peers->rows(peers->count) = row
            peers->cols(peers->count) = c
            peers->count += 1
        End If
    Next

    ' Same column (8 cells)
    For r As Integer = 0 To 8
        If r <> row Then
            peers->rows(peers->count) = r
            peers->cols(peers->count) = col
            peers->count += 1
        End If
    Next

    ' Same 3x3 box (4 cells)
    Dim As Integer box_row = (row \ 3) * 3
    Dim As Integer box_col = (col \ 3) * 3
    For r As Integer = box_row To box_row + 2
        For c As Integer = box_col To box_col + 2
            If r <> row And c <> col Then
                peers->rows(peers->count) = r
                peers->cols(peers->count) = c
                peers->count += 1
            End If
        Next
    Next
End Sub

Sub init_grid(grid As CPGrid Ptr)
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            If puzzle(row, col) = 0 Then
                ' Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid->values(row, col) = 0
                grid->candidates(row, col) = &h3FE  ' Binary: 0011 1111 1110 (bits 1-9)
            Else
                ' Given clue: set single value
                Dim As Integer digit = puzzle(row, col)
                grid->values(row, col) = digit
                grid->candidates(row, col) = (1 Shl digit)
            End If
        Next
    Next
End Sub

' Forward declarations
Declare Function assign(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer

Function eliminate(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer
    ' Check if digit is already eliminated
    If Not has_candidate(grid->candidates(row, col), digit) Then
        Return 1  ' Already eliminated, no change
    End If

    ' Remove digit from candidates
    grid->candidates(row, col) = remove_candidate(grid->candidates(row, col), digit)

    ' Check for contradiction (no candidates left)
    Dim As Integer remaining = count_candidates(grid->candidates(row, col))
    If remaining = 0 Then
        Return 0  ' Contradiction
    End If

    ' If only one candidate left, assign it (singleton elimination)
    If remaining = 1 And grid->values(row, col) = 0 Then
        Dim As Integer last_digit = get_first_candidate(grid->candidates(row, col))
        If Not assign(grid, row, col, last_digit) Then
            Return 0  ' Assignment caused contradiction
        End If
    End If

    Return 1
End Function

Function assign(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer
    ' Increment iteration counter
    cp_iterations += 1

    ' Set value
    grid->values(row, col) = digit
    grid->candidates(row, col) = (1 Shl digit)

    ' Eliminate digit from all peers
    Dim As PeerList peers
    get_peers(row, col, @peers)

    For i As Integer = 0 To peers.count - 1
        If Not eliminate(grid, peers.rows(i), peers.cols(i), digit) Then
            Return 0  ' Contradiction in peer elimination
        End If
    Next

    Return 1
End Function

Function propagate(grid As CPGrid Ptr) As Integer
    Dim As Integer changed = 1

    While changed
        changed = 0

        ' Strategy 1: Singleton elimination
        For row As Integer = 0 To 8
            For col As Integer = 0 To 8
                If grid->values(row, col) = 0 Then
                    Dim As Integer num_candidates = count_candidates(grid->candidates(row, col))
                    If num_candidates = 0 Then
                        Return 0  ' Contradiction
                    End If
                    If num_candidates = 1 Then
                        Dim As Integer digit = get_first_candidate(grid->candidates(row, col))
                        If Not assign(grid, row, col, digit) Then
                            Return 0  ' Assignment caused contradiction
                        End If
                        changed = 1
                    End If
                End If
            Next
        Next

        ' Strategy 2: Hidden singles - rows
        For row As Integer = 0 To 8
            For digit As Integer = 1 To 9
                Dim As Integer count = 0
                Dim As Integer last_col = -1
                Dim As Integer already_assigned = 0

                For col As Integer = 0 To 8
                    If grid->values(row, col) = digit Then
                        already_assigned = 1
                        Exit For
                    End If
                    If has_candidate(grid->candidates(row, col), digit) Then
                        count += 1
                        last_col = col
                    End If
                Next

                If already_assigned Then Continue For

                If count = 1 Then
                    If Not assign(grid, row, last_col, digit) Then
                        Return 0
                    End If
                    changed = 1
                ElseIf count = 0 Then
                    Return 0  ' Digit cannot be placed anywhere in row
                End If
            Next
        Next

        ' Strategy 2: Hidden singles - columns
        For col As Integer = 0 To 8
            For digit As Integer = 1 To 9
                Dim As Integer count = 0
                Dim As Integer last_row = -1
                Dim As Integer already_assigned = 0

                For row As Integer = 0 To 8
                    If grid->values(row, col) = digit Then
                        already_assigned = 1
                        Exit For
                    End If
                    If has_candidate(grid->candidates(row, col), digit) Then
                        count += 1
                        last_row = row
                    End If
                Next

                If already_assigned Then Continue For

                If count = 1 Then
                    If Not assign(grid, last_row, col, digit) Then
                        Return 0
                    End If
                    changed = 1
                ElseIf count = 0 Then
                    Return 0  ' Digit cannot be placed anywhere in column
                End If
            Next
        Next

        ' Strategy 2: Hidden singles - boxes
        For box As Integer = 0 To 8
            Dim As Integer box_row = (box \ 3) * 3
            Dim As Integer box_col = (box Mod 3) * 3

            For digit As Integer = 1 To 9
                Dim As Integer count = 0
                Dim As Integer last_r = -1
                Dim As Integer last_c = -1
                Dim As Integer already_assigned = 0

                For r As Integer = box_row To box_row + 2
                    For c As Integer = box_col To box_col + 2
                        If grid->values(r, c) = digit Then
                            already_assigned = 1
                            Exit For, For
                        End If
                        If has_candidate(grid->candidates(r, c), digit) Then
                            count += 1
                            last_r = r
                            last_c = c
                        End If
                    Next
                    If already_assigned Then Exit For
                Next

                If already_assigned Then Continue For

                If count = 1 Then
                    If Not assign(grid, last_r, last_c, digit) Then
                        Return 0
                    End If
                    changed = 1
                ElseIf count = 0 Then
                    Return 0  ' Digit cannot be placed anywhere in box
                End If
            Next
        Next
    Wend

    Return 1  ' Success - reached fixpoint
End Function

Sub find_mrv_cell(grid As CPGrid Ptr, mrv_row As Integer Ptr, mrv_col As Integer Ptr, found As Integer Ptr)
    Dim As Integer min_candidates = 10
    *found = 0
    *mrv_row = -1
    *mrv_col = -1

    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            If grid->values(r, c) = 0 Then
                Dim As Integer num_candidates = count_candidates(grid->candidates(r, c))
                If num_candidates < min_candidates Then
                    min_candidates = num_candidates
                    *mrv_row = r
                    *mrv_col = c
                    *found = 1
                End If
            End If
        Next
    Next
End Sub

Sub copy_grid(dest As CPGrid Ptr, src As CPGrid Ptr)
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            dest->values(r, c) = src->values(r, c)
            dest->candidates(r, c) = src->candidates(r, c)
        Next
    Next
End Sub

Function cp_search(grid As CPGrid Ptr) As Integer
    ' Base case: check if grid is complete
    Dim As Integer mrv_row, mrv_col, found
    find_mrv_cell(grid, @mrv_row, @mrv_col, @found)

    If Not found Then
        ' No empty cells - grid is complete, extract solution
        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                solution(r * 9 + c) = grid->values(r, c)
            Next
        Next
        Return 1
    End If

    ' Recursive case: try each candidate for the MRV cell
    Dim As Integer candidates = grid->candidates(mrv_row, mrv_col)

    For digit As Integer = 1 To 9
        If has_candidate(candidates, digit) Then
            ' Save grid state for backtracking
            Dim As CPGrid grid_copy
            copy_grid(@grid_copy, grid)

            ' Try assigning this digit
            If assign(grid, mrv_row, mrv_col, digit) Then
                ' Assignment succeeded, propagate constraints
                If propagate(grid) Then
                    ' Propagation succeeded, recurse
                    If cp_search(grid) Then
                        Return 1  ' Found solution
                    End If
                End If
            End If

            ' Failed - restore grid state and try next candidate
            copy_grid(grid, @grid_copy)
        End If
    Next

    ' All candidates exhausted - dead end
    Return 0
End Function

' ============================================================================
' I/O Functions
' ============================================================================

Sub print_puzzle_grid(grid() As Integer)
    Print
    Print "Puzzle:"
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            Print Str$(grid(r, c)); " ";
        Next
        Print " "
    Next
End Sub

Function read_matrix_file(filename As String) As Integer
    Dim f As Integer = FreeFile
    If Open(filename For Input As #f) <> 0 Then
        Print "Error opening file: "; filename
        Return 0
    End If

    Dim line_text As String
    Dim row As Integer = 0
    While Not Eof(f) And row < 9
        Line Input #f, line_text
        If Len(line_text) > 0 And Left(line_text, 1) <> "#" Then
            Dim col As Integer = 0
            Dim i As Integer = 1
            While i <= Len(line_text) And col < 9
                Dim char_at As String = Mid(line_text, i, 1)
                If char_at >= "0" And char_at <= "9" Then
                    puzzle(row, col) = Val(char_at)
                    col += 1
                End If
                i += 1
            Wend
            If col = 9 Then row += 1
        End If
    Wend
    Close #f
    
    If row <> 9 Then
         Print "Error parsing matrix: Found "; row; " rows"
         Return 0
    End If
    
    Return 1
End Function

' ============================================================================
' Main Program
' ============================================================================

Dim As Double start_time, end_time

start_time = Timer

If Command$(1) = "" Then
    Print "Usage: "; Command$(0); " <matrix_file>"
    End 1
End If

' Read puzzle from file
Print "DEBUG: Reading file "; Command$(1)
If Not read_matrix_file(Command$(1)) Then
    Print "DEBUG: Failed to read matrix file"
    End 1
End If

Print "DEBUG: Before print_puzzle_grid"
print_puzzle_grid(puzzle())
Print "DEBUG: After print_puzzle_grid"

' Initialize CP grid
Dim As CPGrid grid
init_grid(@grid)

' Apply initial propagation
Print "DEBUG: Initial propagation"
cp_iterations = 0
If Not propagate(@grid) Then
    Print
    Print "No solution found (contradiction during initial propagation)"
    Print
    end_time = Timer
    Print "Seconds to process "; Format$(end_time - start_time, "0.000")
    End 0
End If

Print "DEBUG: Starting search"

' Run search
Dim As Integer solved = cp_search(@grid)

If solved Then
    ' Convert solution array back to 2D for printing
    Dim As Integer solution_grid(0 To 8, 0 To 8)
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            solution_grid(r, c) = solution(r * 9 + c)
        Next
    Next

    print_puzzle_grid(solution_grid())
    Print
    Print "Solved in Iterations="; cp_iterations
    Print
Else
    Print
    Print "No solution found"
    Print
End If

end_time = Timer
Print "Seconds to process "; Format$(end_time - start_time, "0.000")

End 0