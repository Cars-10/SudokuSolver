' DLX Sudoku Solver in FreeBASIC
' Ported from C implementation - Static Allocation Version

' Types
Type DlxNode
    left As DlxNode Ptr
    right As DlxNode Ptr
    up As DlxNode Ptr
    down As DlxNode Ptr
    column As Any Ptr ' Cast to DlxColumn Ptr
    row_id As Integer
End Type

Type DlxColumn
    node As DlxNode ' Header node
    size As Integer
    name As String * 16
End Type

Type RowInfo
    r As Integer
    c As Integer
    n As Integer
End Type

' Globals
Dim Shared puzzle(9, 9) As Integer
Dim Shared solution_grid(9, 9) As Integer
Dim Shared dlx_iterations As LongInt

' Static Pools
Const MAX_NODES = 5000
Dim Shared node_pool(MAX_NODES) As DlxNode
Dim Shared col_headers(324) As DlxColumn
Dim Shared root As DlxColumn

Dim Shared node_count As Integer
Dim Shared row_info(729) As RowInfo
Dim Shared row_starts(729) As DlxNode Ptr
Dim Shared solution(81) As Integer

' Function Declarations
Declare Sub init_dlx()
Declare Sub cover(c As DlxColumn Ptr)
Declare Sub uncover(c As DlxColumn Ptr)
Declare Function search(k As Integer) As Integer
Declare Sub build_matrix()
Declare Sub solve_dlx()
Declare Sub print_puzzle(grid(any, any) As Integer)
Declare Function read_matrix(filename As String) As Integer

' --- DLX Core Logic ---

Sub cover(c As DlxColumn Ptr)
    c->node.right->left = c->node.left
    c->node.left->right = c->node.right
    
    Dim As DlxNode Ptr i = c->node.down
    While i <> @(c->node)
        Dim As DlxNode Ptr j = i->right
        While j <> i
            j->down->up = j->up
            j->up->down = j->down
            Dim As DlxColumn Ptr col = Cast(DlxColumn Ptr, j->column)
            col->size = col->size - 1
            j = j->right
        Wend
        i = i->down
    Wend
End Sub

Sub uncover(c As DlxColumn Ptr)
    Dim As DlxNode Ptr i = c->node.up
    While i <> @(c->node)
        Dim As DlxNode Ptr j = i->left
        While j <> i
            Dim As DlxColumn Ptr col = Cast(DlxColumn Ptr, j->column)
            col->size = col->size + 1
            j->down->up = j
            j->up->down = j
            j = j->left
        Wend
        i = i->up
    Wend
    
    c->node.right->left = @(c->node)
    c->node.left->right = @(c->node)
End Sub

Function search(k As Integer) As Integer
    dlx_iterations = dlx_iterations + 1
    
    If root.node.right = @(root.node) Then
        Return 1 ' Found solution
    End If
    
    ' Choose column deterministically (smallest size)
    Dim As DlxColumn Ptr c = 0
    Dim As Integer min_size = 999999
    
    Dim As DlxColumn Ptr curr = Cast(DlxColumn Ptr, root.node.right)
    While curr <> @root
        If curr->size < min_size Then
            min_size = curr->size
            c = curr
        End If
        curr = Cast(DlxColumn Ptr, curr->node.right)
    Wend
    
    cover(c)
    
    Dim As DlxNode Ptr r = c->node.down
    While r <> @(c->node)
        solution(k) = r->row_id
        
        Dim As DlxNode Ptr j = r->right
        While j <> r
            cover(Cast(DlxColumn Ptr, j->column))
            j = j->right
        Wend
        
        If search(k + 1) = 1 Then Return 1
        
        ' Recover loop logic
        Dim As DlxNode Ptr next_r = r->down
        
        ' Restore j
        j = r->left
        While j <> r
            uncover(Cast(DlxColumn Ptr, j->column))
            j = j->left
        Wend
        
        r = next_r
    Wend
    
    uncover(c)
    Return 0
End Function

' --- Matrix Setup ---

Sub init_dlx()
    node_count = 0
    
    ' Zero out memory (simplified, assume re-run clears logic or we clear manually)
    ' Actually, for single run per process, BSS is zeroed.
    ' If we re-run in loop (we don't here), we'd need to memset.
    ' But wait, main calls init_dlx(). It's fine.
    
    ' Root setup
    root.node.left = @(root.node)
    root.node.right = @(root.node)
    root.node.up = @(root.node)
    root.node.down = @(root.node)
    root.node.column = @root
    root.node.row_id = -1
    
    Dim As DlxNode Ptr prev = @(root.node)
    
    For i As Integer = 0 To 323
        col_headers(i).name = "C" & Str(i)
        col_headers(i).size = 0
        col_headers(i).node.up = @(col_headers(i).node)
        col_headers(i).node.down = @(col_headers(i).node)
        col_headers(i).node.column = @col_headers(i)
        col_headers(i).node.row_id = -1
        
        col_headers(i).node.left = prev
        col_headers(i).node.right = @(root.node)
        prev->right = @(col_headers(i).node)
        root.node.left = @(col_headers(i).node)
        
        prev = @(col_headers(i).node)
    Next
End Sub

Function add_node(c As DlxColumn Ptr, r_id As Integer) As DlxNode Ptr
    Dim As DlxNode Ptr n = @node_pool(node_count)
    node_count += 1
    
    n->column = c
    n->row_id = r_id
    
    n->down = @(c->node)
    n->up = c->node.up
    c->node.up->down = n
    c->node.up = n
    c->size += 1
    
    Return n
End Function

Sub build_row(r As Integer, c As Integer, n As Integer, r_id As Integer)
    row_info(r_id).r = r
    row_info(r_id).c = c
    row_info(r_id).n = n
    
    Dim As Integer c1 = r * 9 + c
    Dim As Integer c2 = 81 + r * 9 + (n - 1)
    Dim As Integer c3 = 162 + c * 9 + (n - 1)
    Dim As Integer box = (r \ 3) * 3 + (c \ 3)
    Dim As Integer c4 = 243 + box * 9 + (n - 1)
    
    Dim As DlxNode Ptr n1 = add_node(@col_headers(c1), r_id)
    Dim As DlxNode Ptr n2 = add_node(@col_headers(c2), r_id)
    Dim As DlxNode Ptr n3 = add_node(@col_headers(c3), r_id)
    Dim As DlxNode Ptr n4 = add_node(@col_headers(c4), r_id)
    
    n1->right = n2
    n2->right = n3
    n3->right = n4
    n4->right = n1
    
    n1->left = n4
    n2->left = n1
    n3->left = n2
    n4->left = n3
    
    row_starts(r_id) = n1
End Sub

Sub build_matrix()
    Dim As Integer r_id = 0
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            If puzzle(r, c) <> 0 Then
                build_row(r, c, puzzle(r, c), r_id)
                r_id += 1
            Else
                For n As Integer = 1 To 9
                    build_row(r, c, n, r_id)
                    r_id += 1
                Next
            End If
        Next
    Next
End Sub

Sub solve_dlx()
    dlx_iterations = 0
    
    ' Cover clues
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            If puzzle(r, c) <> 0 Then
                Dim As Integer num = puzzle(r, c)
                For i As Integer = 0 To 728
                    If row_starts(i) <> 0 Andalso _
                       row_info(i).r = r Andalso _
                       row_info(i).c = c Andalso _
                       row_info(i).n = num Then
                        
                        Dim As DlxNode Ptr curr = row_starts(i)
                        cover(Cast(DlxColumn Ptr, curr->column))
                        curr = curr->right
                        While curr <> row_starts(i)
                            cover(Cast(DlxColumn Ptr, curr->column))
                            curr = curr->right
                        Wend
                        Exit For
                    End If
                Next
            End If
        Next
    Next
    
    If search(0) = 1 Then
        ' Extract solution
        For r As Integer = 0 To 8
            For c As Integer = 0 To 8
                solution_grid(r, c) = puzzle(r, c)
            Next
        Next
        
        For k As Integer = 0 To 80
            If solution(k) <> -1 Then
                Dim As Integer rid = solution(k)
                solution_grid(row_info(rid).r, row_info(rid).c) = row_info(rid).n
            End If
        Next
    End If
End Sub

Sub print_puzzle(grid(any, any) As Integer)
    Print "Puzzle:"
    For r As Integer = 0 To 8
        For c As Integer = 0 To 8
            Print grid(r, c); " ";
        Next
        Print ""
    Next
End Sub

Function read_matrix(filename As String) As Integer
    Dim f As Integer = FreeFile
    If Open(filename For Input As #f) <> 0 Then
        Return 0
    End If
    
    Dim count As Integer = 0
    While Not Eof(f) And count < 9
        Dim line_str As String
        Line Input #f, line_str
        
        If Left(line_str, 1) = "#" Or Len(line_str) < 9 Then Continue While
        
        Dim idx As Integer = 0
        Dim col_idx As Integer = 0
        
        For i As Integer = 1 To Len(line_str)
            Dim char_code As Integer = Asc(Mid(line_str, i, 1))
            If char_code >= 48 And char_code <= 57 Then
                puzzle(count, col_idx) = char_code - 48
                col_idx += 1
            End If
        Next
        
        If col_idx >= 9 Then count += 1
    Wend
    
    Close #f
    Return 1
End Function

' --- Main ---

Dim As Double start_time, end_time
start_time = Timer

Dim As Integer i
For i = 1 To __FB_ARGC__ - 1
    Dim As String arg = Command(i)
    
    If Right(arg, 7) = ".matrix" Then
        Dim As String display_path = arg
        Dim As Integer idx = Instr(arg, "Matrices/")
        If idx > 0 Then
            display_path = "../" & Mid(arg, idx)
        End If
        Print display_path
        
        If read_matrix(arg) Then
            print_puzzle(puzzle())
            
            init_dlx()
            build_matrix()
            
            For k As Integer = 0 To 80
                solution(k) = -1
            Next
            
            solve_dlx()
            
            Print
            print_puzzle(solution_grid())
            Print
            Print "Solved in Iterations=" & dlx_iterations
            Print
        Else
            Print "Error reading " & arg
        End If
    End If
Next

end_time = Timer
Print Using "Seconds to process #.###"; end_time - start_time
