using System;
using System.IO;
using System.Diagnostics;

namespace Sudoku 
{
    class Sudoku {
    public static Stopwatch s = Stopwatch.StartNew();
    public static int[,] puzzle = new int[9,9];
    public static int count;
    public static int DEBUG = 0; //0 off, 1 High Level, 3 Low Level
    static int readMatrixFile(string filename) {
        if (!File.Exists(filename))  {
            Console.WriteLine("File foes not exist: {0}", filename); 
            return 1;
        }
    
        string[] lines = System.IO.File.ReadAllLines(filename);
        int i = 0; 
        foreach (string line in lines) {
            if (!line.StartsWith("#")) {
                int j = 0;
                foreach (string val in line.Split(" ")) {
                    puzzle[i,j] = int.Parse(val);
                    j++;
                }
            i++;
            }
        }
        return 0;
    }
        
    static void printPuzzle() {
        Console.WriteLine("\nPuzzle:");
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                Console.Write("{0} ", puzzle[j,i]);
            }
            Console.WriteLine("");
        }
        Console.WriteLine("");
    }
    static int isPossible(int y, int x, int val) {
        if (DEBUG>0) Console.WriteLine("Is possible {0:N}, {0:N}, {0:N} count={0:N}" ,x ,y ,val, count);
        // Find if a matching number (val) already exists
        // in the same row (y) or column (x) or within its rectangle
        for (int i = 0; i <9; i++) if(puzzle[i,x] == val) return 0; 
        for (int i = 0; i <9; i++) if(puzzle[y,i] == val) return 0; 
    
        // Search the Rectangle containing x & y
        // Find which 3x3 square we are in using the floor quotient
        int x0=(x/3)*3;
        int y0=(y/3)*3;
        if (DEBUG>0) Console.WriteLine("Is possible x={0:N} x0={0:N}, y={0:N} y0={0:N}, val={0:N}" , x, x0, y, y0, val);

        for (int i = 0; i <3; i++) {
            for (int j = 0; j <3; j++) {
                //if (DEBUG>0) Console.WriteLine("y0+i={0:N} i={0:N}, x0+j={0:N} j={0:N} Puzzle[y0+i][x0+j]={0:N}, val={0:N}", y0+i,i, x0+j,j, puzzle[y0+i,x0+j] , val);
                if(puzzle[y0+i,x0+j] == val ) return 0; 
            }
        }
        if (DEBUG>0) Console.WriteLine("YES possible {0:N}, {0:N}, {0:N}" ,x ,y ,val);
        return 1;
    } 
    static int solve() {
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                 if (DEBUG > 0) Console.WriteLine("Solve: j={0:N},i={0:N}:{0:N}" ,j,i,puzzle[i,j]);
                 if (puzzle[j,i] == 0) {
                    for (int val = 1; val < 10; val++) {
                        count += 1;
                        if (DEBUG>0) Console.WriteLine("Count= :0:N}\n",count);
                        if (isPossible(j,i,val) == 1) {
                            puzzle[j,i] = val;
                            if(solve() == 2) return 2; //Makes sure to do a quick exit when solution was found
                            puzzle[j,i] = 0;
                        }
                    }
                    return 0;
                }
            }
        }
        printPuzzle();
        Console.WriteLine("Solved in Iterations={0}\n", count);
        return 2;
    }
    static void Main(string[] args) {
        string[] arguments = Environment.GetCommandLineArgs();
        if (DEBUG>0) Console.WriteLine("GetCommandLineArgs: {0}", string.Join(", ", arguments));
        if (DEBUG>0) Console.WriteLine("Current Dir: {0}", Directory.GetCurrentDirectory());

        foreach (string arg in arguments) {
            if (arg.EndsWith(".matrix")) {
                Console.WriteLine("File: {0}", arg);
                readMatrixFile(arg);
                printPuzzle(); 
                count = 0;
                solve();
            }
        }
    Console.WriteLine("Seconds to process {0:N}", s.ElapsedMilliseconds/1000.0); 
    }
}
}