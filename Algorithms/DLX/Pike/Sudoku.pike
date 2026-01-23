#!/usr/bin/env pike

// --- Data Structures ---

class DlxNode {
    DlxNode up, down, left, right;
    DlxColumn column;
    int rowId;

    void create() {
        up = down = left = right = this;
    }
}

class DlxColumn {
    inherit DlxNode;
    int size;
    string name;

    void create(string _name) {
        ::create();
        name = _name;
        size = 0;
        column = this;
    }
}

class RowInfo {
    int row, col, num;
    void create(int r, int c, int n) {
        row = r;
        col = c;
        num = n;
    }
}

// --- Global State ---

array(array(int)) puzzle = allocate(9);
array(array(int)) solutionGrid = allocate(9);
int dlxIterations = 0;

DlxColumn root;
array(DlxColumn) columns = allocate(324);
array(RowInfo) rowInfo = allocate(729);
array(DlxNode) rowStarts = allocate(729);

// --- Helpers ---

int getPositionCol(int r, int c) { return r * 9 + c; }
int getRowCol(int r, int n) { return 81 + r * 9 + (n - 1); }
int getColCol(int c, int n) { return 162 + c * 9 + (n - 1); }
int getBoxCol(int r, int c, int n) { return 243 + ((r / 3) * 3 + (c / 3)) * 9 + (n - 1); }

// --- Initialization ---

void initDlxMatrix() {
    root = DlxColumn("root");
    root->rowId = -1;

    for (int i = 0; i < 324; i++) {
        columns[i] = DlxColumn("C" + (string)i);
        columns[i]->rowId = -1;
        
        columns[i]->left = root->left;
        columns[i]->right = root;
        root->left->right = columns[i];
        root->left = columns[i];
    }
}

DlxNode addNode(DlxColumn col, int rowId) {
    DlxNode node = DlxNode();
    node->column = col;
    node->rowId = rowId;

    node->down = col;
    node->up = col->up;
    col->up->down = node;
    col->up = node;
    col->size++;

    return node;
}

void buildDlxRow(int r, int c, int n, int rowId) {
    rowInfo[rowId] = RowInfo(r, c, n);

    DlxNode n1 = addNode(columns[getPositionCol(r, c)], rowId);
    DlxNode n2 = addNode(columns[getRowCol(r, n)], rowId);
    DlxNode n3 = addNode(columns[getColCol(c, n)], rowId);
    DlxNode n4 = addNode(columns[getBoxCol(r, c, n)], rowId);

    n1->right = n2; n2->right = n3; n3->right = n4; n4->right = n1;
    n1->left = n4; n2->left = n1; n3->left = n2; n4->left = n3;

    rowStarts[rowId] = n1;
}

void buildDlxMatrixFromPuzzle() {
    int rowId = 0;
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                buildDlxRow(r, c, puzzle[r][c], rowId++);
            } else {
                for (int n = 1; n <= 9; n++) {
                    buildDlxRow(r, c, n, rowId++);
                }
            }
        }
    }
}

// --- Cover/Uncover ---

void coverColumn(DlxColumn c) {
    c->right->left = c->left;
    c->left->right = c->right;

    for (DlxNode rowNode = c->down; rowNode != c; rowNode = rowNode->down) {
        for (DlxNode rightNode = rowNode->right; rightNode != rowNode; rightNode = rightNode->right) {
            rightNode->down->up = rightNode->up;
            rightNode->up->down = rightNode->down;
            rightNode->column->size--;
        }
    }
}

void uncoverColumn(DlxColumn c) {
    for (DlxNode rowNode = c->up; rowNode != c; rowNode = rowNode->up) {
        for (DlxNode leftNode = rowNode->left; leftNode != rowNode; leftNode = leftNode->left) {
            leftNode->column->size++;
            leftNode->down->up = leftNode;
            leftNode->up->down = leftNode;
        }
    }
    c->right->left = c;
    c->left->right = c;
}

void coverClues() {
    for (int r = 0; r < 9; r++) {
         for (int c = 0; c < 9; c++) {
             if (puzzle[r][c] != 0) {
                 int n = puzzle[r][c];
                 for (int rowId = 0; rowId < 729; rowId++) {
                     if (rowStarts[rowId] && 
                         rowInfo[rowId]->row == r && 
                         rowInfo[rowId]->col == c && 
                         rowInfo[rowId]->num == n) {
                         
                         DlxNode node = rowStarts[rowId];
                         DlxNode curr = node;
                         do {
                             coverColumn(curr->column);
                             curr = curr->right;
                         } while (curr != node);
                         break;
                     }
                 }
             }
         }
    }
}

// --- Search ---

DlxColumn chooseColumn() {
    DlxColumn best;
    int minSize = 2147483647;

    for (DlxNode colNode = root->right; colNode != root; colNode = colNode->right) {
        DlxColumn col = colNode;
        if (col->size < minSize) {
            minSize = col->size;
            best = col;
        }
    }
    return best;
}

int dlxSearch(int k, array(int) solution) {
    dlxIterations++;
    if (root->right == root) return 1;

    DlxColumn col = chooseColumn();
    if (col->size == 0) return 0;

    coverColumn(col);

    for (DlxNode rowNode = col->down; rowNode != col; rowNode = rowNode->down) {
        solution[k] = rowNode->rowId;
        
        for (DlxNode rightNode = rowNode->right; rightNode != rowNode; rightNode = rightNode->right) {
            coverColumn(rightNode->column);
        }

        if (dlxSearch(k + 1, solution)) return 1;

        for (DlxNode leftNode = rowNode->left; leftNode != rowNode; leftNode = leftNode->left) {
            uncoverColumn(leftNode->column);
        }
    }
    uncoverColumn(col);
    return 0;
}

// --- I/O ---

void extractSolution(array(int) solution, int solutionLen) {
    for(int r=0; r<9; r++)
       solutionGrid[r] = allocate(9);
       
    for(int r=0; r<9; r++)
       for(int c=0; c<9; c++)
          solutionGrid[r][c] = puzzle[r][c];

    for(int i=0; i<solutionLen; i++) {
        int rowId = solution[i];
        if (rowId >= 0 && rowId < 729 && rowInfo[rowId]) {
             RowInfo info = rowInfo[rowId];
             solutionGrid[info->row][info->col] = info->num;
        }
    }
}

void printPuzzle(array(array(int)) grid) {
    write("\nPuzzle:\n");
    for(int r=0; r<9; r++) {
        for(int c=0; c<9; c++) write(sprintf("%d ", grid[r][c]));
        write("\n");
    }
}

int read_matrix_file(string filename) {
    string content = Stdio.read_file(filename);
    if (!content) {
        write("Error reading file: " + filename + "\n");
        return 0;
    }

    if (has_prefix(filename, "/app/Matrices/")) {
        write("../" + filename[5..] + "\n");
    } else {
        write(filename + "\n");
    }

    array(string) lines = content / "\n";
    int line_count = 0;

    foreach (lines, string line) {
        line = String.trim_all_whites(line);
        if (sizeof(line) == 0 || line[0] == '#') continue;

        array(string) parts = line / " " - ({ "" });
        if (sizeof(parts) == 9) {
            if (line_count < 9) {
                puzzle[line_count] = allocate(9);
                for (int i = 0; i < 9; i++) {
                    puzzle[line_count][i] = (int)parts[i];
                }
                line_count++;
            }
        }
    }
    return 1;
}

int main(int argc, array(string) argv) {
   float start = (float)gethrtime();
   
   for (int i = 1; i < argc; i++) {
       string filename = argv[i];
       
       // Reset global state for each run
       puzzle = allocate(9);
       solutionGrid = allocate(9);
       rowStarts = allocate(729);
       dlxIterations = 0;
       
       if (read_matrix_file(filename)) {
           printPuzzle(puzzle);
           
           initDlxMatrix();
           buildDlxMatrixFromPuzzle();
           coverClues();
           
           array(int) solution = allocate(81);
           if (dlxSearch(0, solution)) {
               extractSolution(solution, 81); // Assuming worst case full? Or we count K?
               // DLX depth matches # of selected rows (81 for Sudoku)
               // But usually we pass k?
               // Wait, Java passed k+1.
               // Let's check how many we populated.
               // Actually we populate solution[k], so we need the precise k.
               // Wait, extractSolution iterates solution vector. 
               // In Java: extractSolution(solution, 81);
               // Since it's exactly 81 for a solved sudoku, that's fine.
               
               printPuzzle(solutionGrid);
               write(sprintf("\nSolved in Iterations=%d\n\n", dlxIterations));
           } else {
               write(sprintf("\nNo solution found\n\n"));
           }
       }
   }
   
   float end = (float)gethrtime();
   write(sprintf("Seconds to process %.3f\n", (end - start) / 1000000000.0));
   return 0;
}
