#include <stdio.h>
#include <string.h>
#include <sys/time.h>

// in C Array indexes are [j][i] [row][col]!
int puzzle[9][9];
int count;
int DEBUG = 0; //0 off, 1 High Level, 3 Low Level

void printPuzzle() {
    printf("\nPuzzle:\n");
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            printf("%i ", puzzle[j][i]);
        }
    printf("\n");
    }
}

// int getMatrixValue (int r, int c)  { return puzzle[c + 10*r]; }
int readMatrixFile(char * filename ) {
    FILE *file  = NULL; //for file read
    char *line_buf = NULL;
    size_t line_buf_size = 0;
    int line_count = 0;
    ssize_t line_size;

    file = fopen(filename, "r");
    if (!file)
    {
      fprintf(stderr, "Error opening file '%s'\n", filename);
      return 1;
    }
    /* Get the first line of the file. */
    line_size = getline(&line_buf, &line_buf_size, file);
    /* Loop through until we are done with the file. */
    while (line_size >= 0)
    {
        // Skip lines with comments
        if(line_buf[0] != '#') 
        {
            /* Show the line details */
            if (DEBUG==3) printf("line[%06d]: chars=%06zd, buf size=%06zu, contents: %s", line_count,
              line_size, line_buf_size, line_buf);

            if (line_buf_size == 32)
            {
                sscanf ( line_buf , "%i %i %i %i %i %i %i %i %i", &puzzle[line_count][0], &puzzle[line_count][1], 
                &puzzle[line_count][2], &puzzle[line_count][3], &puzzle[line_count][4], &puzzle[line_count][5], 
                &puzzle[line_count][6], &puzzle[line_count][7], &puzzle[line_count][8] 
                );
                if (DEBUG==3) {
                    for (int i = 0; i < 10; i++) printf("%i " , puzzle[line_count][i]);
                    printf("\n");
                }
        
                /* Increment our line count */
                line_count++;
            } else {
                // Something is not kosher
                printf("This line does not appear right\nline[%06d]: chars=%06zd, buf size=%06zu, contents: %s", line_count,
                line_size, line_buf_size, line_buf);
                return 1;
            }
        }
        /* Get the next line */
        line_size = getline(&line_buf, &line_buf_size, file);
    }
    return 0;
}

int floor_div(int a, int b) {
    int d = a / b;
    int r = a % b;  /* optimizes into single division. */
    return r ? (d - ((a < 0) ^ (b < 0))) : d;
}

int isPossible(int y, int x, int val) {
    if (DEBUG) printf("Is possible %i, %i, %i count=%i\n" ,x ,y ,val, count);
    // Find if a matching number (val) already exists
    // in the same row (y) or column (x) or within its rectangle
    for (int i = 0; i <9; i++) if(puzzle[i][x] == val) return 0; 
    for (int i = 0; i <9; i++) if(puzzle[y][i] == val) return 0; 
    
    // Search the Rectangle containing x & y
    // Find which 3x3 square we are in using the floor quotient
    int x0= (floor_div(x,3))*3;
    int y0= (floor_div(y,3))*3;
    if (DEBUG) printf("Is possible x=%i x0=%i, y=%i y0=%i, val=%i\n" , x, x0, y, y0, val);

    for (int i = 0; i <3; i++)
    {
        for (int j = 0; j <3; j++)
        {
            if (DEBUG) printf("y0+i=%i i=%i, x0+j=%i j=%i Puzzle[y0+i][x0+j]=%i, val=%i\n", y0+i,i, x0+j,j, puzzle[y0+i][x0+j] , val);
            if(puzzle[y0+i][x0+j] == val ) return 0; 
        }
    }
    if (DEBUG) printf("YES possible %i, %i, %i\n" ,x ,y ,val);
    return 1;
}

int solve() {
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            if (DEBUG) printf("i=%i,j=%i:%i\n" ,i,j,puzzle[i][j]);
            if (puzzle[j][i] == 0) {
                for (int val = 1; val < 10; val++) {
                    count += 1;
                    if (count==117) printPuzzle();
                    if (isPossible(j,i,val) == 1)
                    {
                        puzzle[j][i] = val;
                        if(solve() == 2) return 2; //Makes sure to do a quick exit when solution was found
                        puzzle[j][i] = 0;
                    }
                }
                return 0;
            }
        }
    }
    printPuzzle();
    printf("\nSolved in Iterations=%i\n\n", count);
    return 2;
}

int main(int argc, char** argv) {
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    // For each .matrix file supplied on the commandline run the solver
    int i;
    char * point;
    for (i = 0; i < argc; i++)
    {
        if((point = strrchr(argv[i],'.')) != NULL ) {
            if (DEBUG) printf("Main: i=%i: %s\n", i, argv[i]);
            //Check if supplied filename ends with csv
            if(strcmp(point,".matrix") == 0) {
                printf("%s\n", argv[i]);
                readMatrixFile(argv[i]);
                printPuzzle(); 
            }
            count = 0;
            solve();
        }
    }
    
    gettimeofday(&stop, NULL);
    printf("Seconds to process %.3f\n", (stop.tv_sec - start.tv_sec) + (stop.tv_usec - start.tv_usec)/1000000.0); 
    return 0;
}
