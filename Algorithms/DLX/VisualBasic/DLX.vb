Imports System
Imports System.IO
Imports System.Collections.Generic

' Dancing Links (DLX) Sudoku Solver
' Direct port of C implementation to maintain algorithmic compatibility

Public Class DlxNode
    Public Up As DlxNode
    Public Down As DlxNode
    Public Left As DlxNode
    Public Right As DlxNode
    Public Column As DlxColumn
    Public RowId As Integer
End Class

Public Class DlxColumn
    Inherits DlxNode
    Public Size As Integer
    Public Name As String

    Public Sub New()
        Column = Me
    End Sub
End Class

Public Class RowInfo
    Public Row As Integer
    Public Col As Integer
    Public Num As Integer
End Class

Module Program
    ' Global iteration counter (Module level to mimic static)
    Private dlxIterations As Integer = 0
    
    ' DLX matrix structures
    Private root As DlxColumn
    Private columns(323) As DlxColumn
    Private nodes() As DlxNode
    Private nodeCount As Integer
    Private maxNodes As Integer

    ' Row metadata
    Private rowInfo(728) As RowInfo
    Private rowStarts(728) As DlxNode

    ' Puzzle state
    Private puzzle(8, 8) As Integer
    Private solutionGrid(8, 8) As Integer

    Sub Main(args As String())
        Dim startTime As DateTime = DateTime.Now

        If args.Length < 1 Then
            Console.WriteLine("Usage: dotnet run <matrix_file>")
            Return
        End If

        For Each arg As String In args
             Solve(arg)
        Next

        Dim elapsed As TimeSpan = DateTime.Now - startTime
        Console.WriteLine($"Seconds to process {elapsed.TotalSeconds:F3}")
    End Sub

    ' Calculate constraint column indices
    Function GetPositionCol(r As Integer, c As Integer) As Integer
        Return r * 9 + c
    End Function

    Function GetRowCol(r As Integer, n As Integer) As Integer
        Return 81 + r * 9 + (n - 1)
    End Function

    Function GetColCol(c As Integer, n As Integer) As Integer
        Return 162 + c * 9 + (n - 1)
    End Function

    Function GetBoxCol(r As Integer, c As Integer, n As Integer) As Integer
        Dim box As Integer = (r \ 3) * 3 + (c \ 3)
        Return 243 + box * 9 + (n - 1)
    End Function

    ' Initialize DLX matrix structure
    Sub InitDlxMatrix()
        ' Allocate root column
        root = New DlxColumn()
        root.Name = "root"
        root.Left = root
        root.Right = root
        root.Up = root
        root.Down = root
        root.RowId = -1
        root.Size = 0

        ' Allocate 324 column headers
        For i As Integer = 0 To 323
            columns(i) = New DlxColumn()
            columns(i).Name = $"C{i}"
            columns(i).Size = 0

            ' Initialize as circular list
            columns(i).Up = columns(i)
            columns(i).Down = columns(i)
            columns(i).RowId = -1

            ' Link into header list
            columns(i).Left = root.Left
            columns(i).Right = root
            root.Left.Right = columns(i)
            root.Left = columns(i)
        Next

        ' Initialize node pool
        maxNodes = 729 * 4 ' 4 constraints per row option
        ReDim nodes(maxNodes - 1)
        For i As Integer = 0 To maxNodes - 1
            nodes(i) = New DlxNode()
        Next
        nodeCount = 0

        ' Initialize row info
        For i As Integer = 0 To 728
            rowInfo(i) = New RowInfo()
        Next
        
        ReDim rowStarts(728)
    End Sub

    ' Add a node to the DLX matrix
    Function AddNode(col As DlxColumn, rowId As Integer) As DlxNode
        If nodeCount >= maxNodes Then
            Throw New Exception("Exceeded maximum node count")
        End If

        Dim node As DlxNode = nodes(nodeCount)
        nodeCount += 1
        node.Column = col
        node.RowId = rowId

        ' Insert at end of column's circular list
        node.Down = col
        node.Up = col.Up
        col.Up.Down = node
        col.Up = node
        col.Size += 1

        Return node
    End Function

    ' Build a DLX row for Sudoku cell (r,c) with value n
    Sub BuildDlxRow(r As Integer, c As Integer, n As Integer, rowId As Integer)
        ' Store row metadata
        rowInfo(rowId).Row = r
        rowInfo(rowId).Col = c
        rowInfo(rowId).Num = n

        ' Create nodes for the 4 constraints
        Dim n1 As DlxNode = AddNode(columns(GetPositionCol(r, c)), rowId)
        Dim n2 As DlxNode = AddNode(columns(GetRowCol(r, n)), rowId)
        Dim n3 As DlxNode = AddNode(columns(GetColCol(c, n)), rowId)
        Dim n4 As DlxNode = AddNode(columns(GetBoxCol(r, c, n)), rowId)

        ' Link nodes horizontally in circular list
        n1.Right = n2
        n2.Right = n3
        n3.Right = n4
        n4.Right = n1

        n1.Left = n4
        n2.Left = n1
        n3.Left = n2
        n4.Left = n3

        ' Store first node for this row
        rowStarts(rowId) = n1
    End Sub

    ' Build the complete DLX matrix from the puzzle
    Sub BuildDlxMatrixFromPuzzle()
        Dim rowId As Integer = 0

        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                If puzzle(r, c) <> 0 Then
                    ' Cell has a clue - create only one row for that value
                    BuildDlxRow(r, c, puzzle(r, c), rowId)
                    rowId += 1
                Else
                    ' Cell is empty - create rows for all possible values
                    For n As Integer = 1 To 9
                        BuildDlxRow(r, c, n, rowId)
                        rowId += 1
                    Next
                End If
            Next
        Next
    End Sub

    ' Cover a column in the DLX matrix
    Sub CoverColumn(c As DlxColumn)
        ' Remove column header from the header list
        c.Right.Left = c.Left
        c.Left.Right = c.Right

        ' For each row in this column
        Dim rowNode As DlxNode = c.Down
        While rowNode IsNot c
            ' For each node in this row (excluding the column itself)
            Dim rightNode As DlxNode = rowNode.Right
            While rightNode IsNot rowNode
                ' Remove this node from its column
                rightNode.Down.Up = rightNode.Up
                rightNode.Up.Down = rightNode.Down
                rightNode.Column.Size -= 1
                rightNode = rightNode.Right
            End While
            rowNode = rowNode.Down
        End While
    End Sub

    ' Uncover a column (exact reverse of cover)
    Sub UncoverColumn(c As DlxColumn)
        ' For each row in this column (in reverse order)
        Dim rowNode As DlxNode = c.Up
        While rowNode IsNot c
            ' For each node in this row (in reverse order)
            Dim leftNode As DlxNode = rowNode.Left
            While leftNode IsNot rowNode
                ' Restore this node to its column
                leftNode.Column.Size += 1
                leftNode.Down.Up = leftNode
                leftNode.Up.Down = leftNode
                leftNode = leftNode.Left
            End While
            rowNode = rowNode.Up
        End While

        ' Restore column header to the header list
        c.Right.Left = c
        c.Left.Right = c
    End Sub

    ' Choose column with minimum size (Knuth's S heuristic)
    Function ChooseColumn() As DlxColumn
        Dim best As DlxColumn = Nothing
        Dim minSize As Integer = Integer.MaxValue

        Dim colNode As DlxNode = root.Right
        While colNode IsNot root
            Dim col As DlxColumn = DirectCast(colNode, DlxColumn)
            If col.Size < minSize Then
                minSize = col.Size
                best = col
            End If
            colNode = colNode.Right
        End While

        Return best
    End Function

    ' DLX Search - Algorithm X with Dancing Links
    Function DlxSearch(k As Integer, solution() As Integer) As Boolean
        dlxIterations += 1 ' Count every search call

        ' If matrix is empty, we found a solution
        If root.Right Is root Then
            Return True
        End If

        ' Choose column with minimum size
        Dim col As DlxColumn = ChooseColumn()

        ' If column has no rows, no solution possible
        If col.Size = 0 Then
            Return False
        End If

        ' Cover this column
        CoverColumn(col)

        ' Try each row in this column
        Dim rowNode As DlxNode = col.Down
        While rowNode IsNot col
            ' Add row to partial solution
            solution(k) = rowNode.RowId

            ' Cover all other columns in this row
            Dim rightNode As DlxNode = rowNode.Right
            While rightNode IsNot rowNode
                CoverColumn(rightNode.Column)
                rightNode = rightNode.Right
            End While

            ' Recurse
            If DlxSearch(k + 1, solution) Then
                Return True ' Solution found
            End If

            ' Backtrack: uncover all columns in this row
            Dim leftNode As DlxNode = rowNode.Left
            While leftNode IsNot rowNode
                UncoverColumn(leftNode.Column)
                leftNode = leftNode.Left
            End While

            rowNode = rowNode.Down
        End While

        ' Uncover column
        UncoverColumn(col)

        Return False ' No solution found
    End Function

    ' Cover given clues (pre-selected rows)
    Sub CoverClues()
        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                If puzzle(r, c) <> 0 Then
                    Dim n As Integer = puzzle(r, c)

                    ' Find the row for this clue
                    For rowId As Integer = 0 To 728
                        If rowStarts(rowId) IsNot Nothing AndAlso
                           rowInfo(rowId).Row = r AndAlso
                           rowInfo(rowId).Col = c AndAlso
                           rowInfo(rowId).Num = n Then

                            ' Cover all columns in this row
                            Dim node As DlxNode = rowStarts(rowId)
                            Dim curr As DlxNode = node
                            Do
                                CoverColumn(curr.Column)
                                curr = curr.Right
                            Loop While curr IsNot node
                            Exit For
                        End If
                    Next
                End If
            Next
        Next
    End Sub

    ' Extract solution from DLX and populate solution grid
    Sub ExtractSolution(solution() As Integer, solutionLen As Integer)
        ' Initialize solution grid - start with the original puzzle
        Array.Copy(puzzle, solutionGrid, puzzle.Length)

        ' Each solution entry is a row_id
        For i As Integer = 0 To solutionLen - 1
            Dim rowId As Integer = solution(i)
            If rowId >= 0 AndAlso rowId < 729 Then
                solutionGrid(rowInfo(rowId).Row, rowInfo(rowId).Col) = rowInfo(rowId).Num
            End If
        Next
    End Sub

    ' Print puzzle
    Sub PrintPuzzle(grid(,) As Integer)
        Console.WriteLine()
        Console.WriteLine("Puzzle:")
        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                Console.Write($"{grid(r, c)} ")
            Next
            Console.WriteLine()
        Next
    End Sub

    ' Read matrix file
    Function ReadMatrixFile(filename As String) As Boolean
        If Not File.Exists(filename) Then
            Console.Error.WriteLine($"Error opening file '{filename}'")
            Return False
        End If

        ' Normalize path for output
        Dim displayPath As String = filename
        If filename.StartsWith("/app/Matrices/") Then
            displayPath = filename.Substring(5)
            Console.WriteLine($"../{displayPath}")
        Else
            Console.WriteLine(filename)
        End If

        Dim lineCount As Integer = 0
        For Each line As String In File.ReadLines(filename)
            ' Skip comments and empty lines
            If String.IsNullOrWhiteSpace(line) OrElse line.StartsWith("#") Then
                Continue For
            End If

            Dim parts As String() = line.Split(New Char() {" "c, vbTab}, StringSplitOptions.RemoveEmptyEntries)
            If parts.Length = 9 AndAlso lineCount < 9 Then
                For i As Integer = 0 To 8
                    puzzle(lineCount, i) = Integer.Parse(parts(i))
                    Console.Write($"{puzzle(lineCount, i)} ")
                Next
                Console.WriteLine()
                lineCount += 1
            End If
        Next

        Return lineCount = 9
    End Function

    Sub Solve(filename As String)
        If Not ReadMatrixFile(filename) Then
            Console.Error.WriteLine($"Error reading {filename}")
            Return
        End If

        PrintPuzzle(puzzle)

        ' Initialize DLX matrix
        InitDlxMatrix()

        ' Build matrix from puzzle
        BuildDlxMatrixFromPuzzle()

        ' Cover pre-filled clues
        CoverClues()

        ' Solve using DLX
        dlxIterations = 0
        Dim solution(80) As Integer
        Dim result As Boolean = DlxSearch(0, solution)

        If result Then
            ExtractSolution(solution, 81)
            PrintPuzzle(solutionGrid)
            Console.WriteLine($"{vbLf}Solved in Iterations={dlxIterations}{vbLf}")
        Else
            Console.WriteLine($"{vbLf}No solution found")
        End If
    End Sub

End Module
