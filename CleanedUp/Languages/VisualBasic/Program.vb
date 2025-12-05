Imports System
Imports System.IO

Module Program
    Sub Main(args As String())
        If args.Length < 1 Then
            Console.WriteLine("Usage: dotnet run <matrix_file>")
            Return
        End If

        Dim filename As String = args(0)
        Dim sudoku As New Sudoku()
        
        sudoku.ReadMatrix(filename)
        
        Console.WriteLine("Puzzle:")
        sudoku.PrintBoard()
        
        If sudoku.Solve() Then
            Console.WriteLine("Puzzle:")
            sudoku.PrintBoard()
            Console.WriteLine($"Solved in Iterations={sudoku.GetIterations()}")
        Else
            Console.WriteLine("No solution found.")
        End If
    End Sub
End Module

Public Class Sudoku
    Private board(8, 8) As Integer
    Private iterations As Long = 0

    Public Sub ReadMatrix(filename As String)
        Try
            Dim lines As String() = File.ReadAllLines(filename)
            Dim row As Integer = 0
            
            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) OrElse line.StartsWith("#") Then
                    Continue For
                End If
                
                If row >= 9 Then Exit For
                
                Dim col As Integer = 0
                For i As Integer = 0 To line.Length - 1
                    Dim c As Char = line(i)
                    If Char.IsDigit(c) Then
                        If col < 9 Then
                            board(row, col) = Integer.Parse(c.ToString())
                            col += 1
                        End If
                    ElseIf c = "."c Then
                        If col < 9 Then
                            board(row, col) = 0
                            col += 1
                        End If
                    End If
                Next
                row += 1
            Next
        Catch ex As Exception
            Console.WriteLine($"Error reading file: {ex.Message}")
            Environment.Exit(1)
        End Try
    End Sub

    Public Sub PrintBoard()
        For i As Integer = 0 To 8
            For j As Integer = 0 To 8
                Console.Write($"{board(i, j)} ")
            Next
            Console.WriteLine()
        Next
    End Sub

    Private Function IsValid(row As Integer, col As Integer, num As Integer) As Boolean
        ' Row check
        For c As Integer = 0 To 8
            If board(row, c) = num Then Return False
        Next

        ' Col check
        For r As Integer = 0 To 8
            If board(r, col) = num Then Return False
        Next

        ' Box check
        Dim boxRow As Integer = (row \ 3) * 3
        Dim boxCol As Integer = (col \ 3) * 3

        For r As Integer = 0 To 2
            For c As Integer = 0 To 2
                If board(boxRow + r, boxCol + c) = num Then Return False
            Next
        Next

        Return True
    End Function

    Private Function FindEmpty(ByRef row As Integer, ByRef col As Integer) As Boolean
        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                If board(r, c) = 0 Then
                    row = r
                    col = c
                    Return True
                End If
            Next
        Next
        row = -1
        col = -1
        Return False
    End Function

    Public Function Solve() As Boolean
        Dim row, col As Integer
        
        If Not FindEmpty(row, col) Then
            Return True
        End If

        For num As Integer = 1 To 9
            iterations += 1
            If IsValid(row, col, num) Then
                board(row, col) = num
                
                If Solve() Then
                    Return True
                End If
                
                board(row, col) = 0
            End If
        Next

        Return False
    End Function

    Public Function GetIterations() As Long
        Return iterations
    End Function
End Class
