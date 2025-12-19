' Sudoku Solver in FreeBASIC

Dim Shared board(0 To 8, 0 To 8) As Integer
Dim Shared iterations As LongInt = 0

Sub PrintBoard()
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            Print Trim(Str(board(r, c)));
            If c < 8 Then Print " ";
        Next
        Print
    Next
End Sub

Function IsValid(row As Integer, col As Integer, value As Integer) As Integer
    ' Check row
    For i As Integer = 0 To 8
        If board(row, i) = value Then Return 0
    Next

    ' Check col
    For i As Integer = 0 To 8
        If board(i, col) = value Then Return 0
    Next

    ' Check box
    Dim startRow As Integer = (row \ 3) * 3
    Dim startCol As Integer = (col \ 3) * 3
    For i As Integer = 0 To 2
        For j As Integer = 0 To 2
            If board(startRow + i, startCol + j) = value Then Return 0
        Next
    Next

    Return 1
End Function

Function Solve() As Integer
    Dim row As Integer = -1
    Dim col As Integer = -1

    ' Find first empty cell
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            If board(r, c) = 0 Then
                row = r
                col = c
                Exit For, For
            End If
        Next
    Next

    ' If no empty cell, solved
    If row = -1 Then Return 1

    ' Try values 1-9
    For value As Integer = 1 To 9
        iterations += 1
        If IsValid(row, col, value) Then
            board(row, col) = value
            If Solve() = 1 Then Return 1
            board(row, col) = 0
        End If
    Next

    Return 0
End Function

Sub ReadMatrix(filename As String)
    Dim f As Integer = FreeFile
    If Open(filename For Input As #f) <> 0 Then
        Print "Error opening file: "; filename
        End 1
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
                    board(row, col) = Val(char_at)
                    col += 1
                End If
                i += 1
            Wend
            row += 1
        End If
    Wend
    Close #f
End Sub

' Main
If Command(1) = "" Then
    Print "Usage: Sudoku <matrix_file>"
    End 1
End If

Dim filename As String = Command(1)

' Format path for consistency (matches C solver behavior if possible)
' C solver prints "../Matrices/1.matrix" etc.
Print filename
Print

ReadMatrix(filename)

Print "Puzzle:"
PrintBoard()

If Solve() = 1 Then
    Print
    Print "Puzzle:"
    PrintBoard()
    Print
    Print "Solved in Iterations="; iterations
Else
    Print "No solution found."
End If
