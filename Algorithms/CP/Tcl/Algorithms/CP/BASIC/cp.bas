' Constraint Propagation (CP) Sudoku Solver - FreeBASIC Implementation
' Algorithm: Constraint propagation with MRV heuristic

#include "file.bi"

' Global iteration counter
Dim Shared cp_iterations As Integer = 0

' Grid structure
Type CPGrid
    values(8, 8) As Integer
    candidates(8, 8) As Integer
End Type

' Bitset operations
Function has_candidate(candidate_set As Integer, digit As Integer) As Integer
    Return (candidate_set And (1 Shl digit)) <> 0
End Function

Function remove_candidate(candidate_set As Integer, digit As Integer) As Integer
    Return candidate_set And (Not (1 Shl digit))
End Function

Function count_candidates(candidate_set As Integer) As Integer
    Dim count As Integer = 0
    For i As Integer = 1 To 9
        If has_candidate(candidate_set, i) Then count += 1
    Next
    Return count
End Function

Function get_first_candidate(candidate_set As Integer) As Integer
    For i As Integer = 1 To 9
        If has_candidate(candidate_set, i) Then Return i
    Next
    Return 0
End Function

' Forward declarations
Declare Function assign_value(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer
Declare Function eliminate(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer

' Initialize grid
Sub init_grid(grid As CPGrid Ptr, puzzle() As Integer)
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            If puzzle(row, col) = 0 Then
                grid->values(row, col) = 0
                grid->candidates(row, col) = &h3FE
            Else
                Dim digit As Integer = puzzle(row, col)
                grid->values(row, col) = digit
                grid->candidates(row, col) = (1 Shl digit)
            End If
        Next
    Next
End Sub

' Eliminate digit from candidates
Function eliminate(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer
    If Not has_candidate(grid->candidates(row, col), digit) Then Return 1
    
    grid->candidates(row, col) = remove_candidate(grid->candidates(row, col), digit)
    
    Dim remaining As Integer = count_candidates(grid->candidates(row, col))
    If remaining = 0 Then Return 0
    
    If remaining = 1 And grid->values(row, col) = 0 Then
        Dim last_digit As Integer = get_first_candidate(grid->candidates(row, col))
        If Not assign_value(grid, row, col, last_digit) Then Return 0
    End If
    
    Return 1
End Function

' Assign digit to cell
Function assign_value(grid As CPGrid Ptr, row As Integer, col As Integer, digit As Integer) As Integer
    cp_iterations += 1
    
    grid->values(row, col) = digit
    grid->candidates(row, col) = (1 Shl digit)
    
    ' Eliminate from row
    For c As Integer = 0 To 8
        If c <> col Then
            If Not eliminate(grid, row, c, digit) Then Return 0
        End If
    Next
    
    ' Eliminate from column
    For r As Integer = 0 To 8
        If r <> row Then
            If Not eliminate(grid, r, col, digit) Then Return 0
        End If
    Next
    
    ' Eliminate from box
    Dim box_row As Integer = (row \ 3) * 3
    Dim box_col As Integer = (col \ 3) * 3
    For r As Integer = box_row To box_row + 2
        For c As Integer = box_col To box_col + 2
            If r <> row And c <> col Then
                If Not eliminate(grid, r, c, digit) Then Return 0
            End If
        Next
    Next
    
    Return 1
End Function

' Apply constraint propagation
Function propagate(grid As CPGrid Ptr) As Integer
    Dim changed As Integer = 1
    
    While changed
        changed = 0
        
        ' Singleton elimination
        For row As Integer = 0 To 8
            For col As Integer = 0 To 8
                If grid->values(row, col) = 0 Then
                    Dim num_candidates As Integer = count_candidates(grid->candidates(row, col))
                    If num_candidates = 0 Then Return 0
                    If num_candidates = 1 Then
                        Dim digit As Integer = get_first_candidate(grid->candidates(row, col))
                        If Not assign_value(grid, row, col, digit) Then Return 0
                        changed = 1
                    End If
                End If
            Next
        Next
        
        ' Hidden singles - rows
        For row As Integer = 0 To 8
            For digit As Integer = 1 To 9
                Dim count As Integer = 0
                Dim last_col As Integer = -1
                Dim already_assigned As Integer = 0
                
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
                    If Not assign_value(grid, row, last_col, digit) Then Return 0
                    changed = 1
                ElseIf count = 0 Then
                    Return 0
                End If
            Next
        Next
        
        ' Hidden singles - columns
        For col As Integer = 0 To 8
            For digit As Integer = 1 To 9
                Dim count As Integer = 0
                Dim last_row As Integer = -1
                Dim already_assigned As Integer = 0
                
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
                    If Not assign_value(grid, last_row, col, digit) Then Return 0
                    changed = 1
                ElseIf count = 0 Then
                    Return 0
                End If
            Next
        Next
        
        ' Hidden singles - boxes
        For box As Integer = 0 To 8
            Dim box_row As Integer = (box \ 3) * 3
            Dim box_col As Integer = (box Mod 3) * 3
            
            For digit As Integer = 1 To 9
                Dim count As Integer = 0
                Dim last_r As Integer = -1
                Dim last_c As Integer = -1
                Dim already_assigned As Integer = 0
                
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
                Next
                
                If already_assigned Then Continue For
                
                If count = 1 Then
                    If Not assign_value(grid, last_r, last_c, digit) Then Return 0
                    changed = 1
                ElseIf count = 0 Then
                    Return 0
                End If
            Next
        Next
    Wend
    
    Return 1
End Function

' Check if complete
Function is_complete(grid As CPGrid Ptr) As Integer
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            If grid->values(row, col) = 0 Then Return 0
        Next
    Next
    Return 1
End Function

' Find MRV cell
Sub find_mrv_cell(grid As CPGrid Ptr, ByRef min_row As Integer, ByRef min_col As Integer)
    Dim min_count As Integer = 10
    min_row = -1
    min_col = -1
    
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            If grid->values(row, col) = 0 Then
                Dim count As Integer = count_candidates(grid->candidates(row, col))
                If count < min_count Then
                    min_count = count
                    min_row = row
                    min_col = col
                End If
            End If
        Next
    Next
End Sub

' Save state
Sub save_state(grid As CPGrid Ptr, saved As CPGrid Ptr)
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            saved->values(row, col) = grid->values(row, col)
            saved->candidates(row, col) = grid->candidates(row, col)
        Next
    Next
End Sub

' Restore state
Sub restore_state(grid As CPGrid Ptr, saved As CPGrid Ptr)
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            grid->values(row, col) = saved->values(row, col)
            grid->candidates(row, col) = saved->candidates(row, col)
        Next
    Next
End Sub

' Solve with search
Function cp_search(grid As CPGrid Ptr) As Integer
    If Not propagate(grid) Then Return 0
    If is_complete(grid) Then Return 1
    
    Dim row As Integer, col As Integer
    find_mrv_cell(grid, row, col)
    
    If row = -1 Then Return 0
    
    Dim candidates_set As Integer = grid->candidates(row, col)
    Dim saved As CPGrid
    
    For digit As Integer = 1 To 9
        If has_candidate(candidates_set, digit) Then
            save_state(grid, @saved)
            
            If assign_value(grid, row, col, digit) Then
                If cp_search(grid) Then Return 1
            End If
            
            restore_state(grid, @saved)
        End If
    Next
    
    Return 0
End Function

' Read puzzle
Function read_puzzle(filename As String, puzzle() As Integer) As Integer
    Dim f As Integer = FreeFile
    If Open(filename For Input As #f) <> 0 Then Return 0
    
    Dim row As Integer = 0
    Dim line As String
    
    While Not Eof(f) And row < 9
        Line Input #f, line
        Dim col As Integer = 0
        Dim i As Integer = 1
        
        While i <= Len(line) And col < 9
            Dim ch As String = Mid(line, i, 1)
            If ch >= "0" And ch <= "9" Then
                puzzle(row, col) = Val(ch)
                col += 1
            End If
            i += 1
        Wend
        
        If col = 9 Then row += 1
    Wend
    
    Close #f
    Return 1
End Function

' Print grid
Sub print_grid(grid As CPGrid Ptr)
    Print
    Print "Puzzle:"
    For row As Integer = 0 To 8
        For col As Integer = 0 To 8
            Print grid->values(row, col); " ";
        Next
        Print
    Next
End Sub

' Main
Dim puzzle(8, 8) As Integer
Dim grid As CPGrid

If Command(1) = "" Then
    Print "Usage: cp <matrix_file>"
    End 1
End If

Dim filename As String = Command(1)
Print filename

If Not read_puzzle(filename, puzzle()) Then
    Print "Error reading file"
    End 1
End If

init_grid(@grid, puzzle())
print_grid(@grid)

Dim start_time As Double = Timer
Dim result As Integer = cp_search(@grid)
Dim end_time As Double = Timer

print_grid(@grid)

If result Then
    Print
    Print "Solved in Iterations="; cp_iterations
Else
    Print
    Print "No solution found"
End If

Print
Print "Seconds to process "; Format(end_time - start_time, "0.000")
