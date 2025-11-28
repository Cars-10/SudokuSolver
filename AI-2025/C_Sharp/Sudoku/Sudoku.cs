using System;
using System.IO;
using System.Diagnostics;

namespace Sudoku 
{
    class Sudoku {
        public static Stopwatch s = Stopwatch.StartNew();
        public static int[,] puzzle = new int[9,9];
        public static int count;
        public static int DEBUG = 0;

        static void printPuzzle() {
            Console.WriteLine("\nPuzzle:");
            for (int i = 0; i < 9; i++) {
                for (int j = 0; j < 9; j++) {
                    Console.Write(puzzle[i,j] + " ");
                }
                Console.WriteLine();
            }
        }

        static int readMatrixFile(string filename) {
            try {
                string[] lines = File.ReadAllLines(filename);
                int row = 0;
                foreach (string line in lines) {
                    if (line.StartsWith("#") || string.IsNullOrWhiteSpace(line)) continue;
                    string[] parts = line.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length != 9) continue;
                    for (int col = 0; col < 9; col++) {
                        puzzle[row, col] = int.Parse(parts[col]);
                    }
                    row++;
                    if (row == 9) break;
                }
                return 0;
            } catch (Exception e) {
                Console.WriteLine("Error reading file: " + e.Message);
                return 1;
            }
        }

        static int isPossible(int y, int x, int val) {
            for (int i = 0; i < 9; i++) if(puzzle[i,x] == val) return 0; 
            for (int i = 0; i < 9; i++) if(puzzle[y,i] == val) return 0; 
            int x0=(x/3)*3;
            int y0=(y/3)*3;
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if(puzzle[y0+i,x0+j] == val ) return 0; 
                }
            }
            return 1;
        } 

        static int solve() {
            for (int j = 0; j < 9; j++) {
                for (int i = 0; i < 9; i++) {
                     if (puzzle[j,i] == 0) {
                        for (int val = 1; val < 10; val++) {
                            count += 1;
                            if (isPossible(j,i,val) == 1) {
                                puzzle[j,i] = val;
                                if(solve() == 2) return 2; 
                                puzzle[j,i] = 0;
                            }
                        }
                        return 0;
                    }
                }
            }
            printPuzzle();
            Console.WriteLine("\nSolved in Iterations={0}\n", count);
            return 2;
        }

        static void Main(string[] args) {
            if (args.Length > 0) {
                foreach (string filename in args) {
                    Console.WriteLine("\n{0}", filename);
                    if (readMatrixFile(filename) == 0) {
                        printPuzzle();
                        count = 0;
                        solve();
                    }
                }
            }
            s.Stop();
            Console.WriteLine("Seconds to process {0:F3}", s.ElapsedMilliseconds/1000.0);
        }
    }
}