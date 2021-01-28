using System;
using System.IO;

namespace Sudoku
{
    class Sudoku
    {
        static void Main(string[] args)
        {
            
            string[] arguments = Environment.GetCommandLineArgs();
            Console.WriteLine("GetCommandLineArgs: {0}", string.Join(", ", arguments));
            Console.WriteLine("Current Dir: {0}", Directory.GetCurrentDirectory());

            foreach (string arg in arguments)
            {
                if (arg.EndsWith(".matrix")) {
                    Console.WriteLine("File: {0}", arg);
                }
            }

        }
    }
}
