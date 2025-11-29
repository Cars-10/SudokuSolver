(* Sudoku Solver in Standard ML *)

structure Sudoku = struct

val puzzle = Array2.array(9, 9, 0)
val count = ref 0

fun printPuzzle () =
    (print "\nPuzzle:\n";
     let fun loopR r =
             if r = 9 then ()
             else (let fun loopC c =
                           if c = 9 then ()
                           else (print (Int.toString (Array2.sub(puzzle, r, c)) ^ " ");
                                 loopC (c + 1))
                   in loopC 0; print "\n"; loopR (r + 1) end)
     in loopR 0 end)

fun readMatrixFile filename =
    let val _ = print (filename ^ "\n")
        val ins = TextIO.openIn filename
        fun loop row =
            case TextIO.inputLine ins of
                NONE => ()
              | SOME line =>
                if String.isPrefix "#" line orelse size line < 2 then loop row
                else
                    let val parts = String.tokens (fn c => c = #" " orelse c = #"\t" orelse c = #"\n") line
                    in if length parts = 9 then
                           (List.foldl (fn (s, col) =>
                                           (case Int.fromString s of
                                                SOME v => Array2.update(puzzle, row, col, v)
                                              | NONE => ();
                                            col + 1)) 0 parts;
                            if row < 8 then loop (row + 1) else ())
                       else loop row
                    end
    in loop 0; TextIO.closeIn ins end

fun isPossible (r, c, v) =
    let fun checkRow i =
            if i = 9 then true
            else if Array2.sub(puzzle, i, c) = v orelse Array2.sub(puzzle, r, i) = v then false
            else checkRow (i + 1)
        val r0 = (r div 3) * 3
        val c0 = (c div 3) * 3
        fun checkBox i j =
            if i = 3 then true
            else if j = 3 then checkBox (i + 1) 0
            else if Array2.sub(puzzle, r0 + i, c0 + j) = v then false
            else checkBox i (j + 1)
    in checkRow 0 andalso checkBox 0 0 end

fun solve () =
    let fun findEmpty r c =
            if r = 9 then NONE
            else if c = 9 then findEmpty (r + 1) 0
            else if Array2.sub(puzzle, r, c) = 0 then SOME (r, c)
            else findEmpty r (c + 1)
    in case findEmpty 0 0 of
           NONE => (printPuzzle ();
                    print ("\nSolved in Iterations=" ^ Int.toString (!count) ^ "\n\n");
                    true)
         | SOME (r, c) =>
           let fun tryVal v =
                   if v > 9 then false
                   else (count := !count + 1;
                         if isPossible (r, c, v) then
                             (Array2.update(puzzle, r, c, v);
                              if solve () then true
                              else (Array2.update(puzzle, r, c, 0);
                                    tryVal (v + 1)))
                         else tryVal (v + 1))
           in tryVal 1 end
    end

fun main () =
    let val args = CommandLine.arguments ()
        val start = Time.now ()
        fun processArgs [] = ()
          | processArgs (arg::rest) =
            if String.isSuffix ".matrix" arg then
                (readMatrixFile arg;
                 printPuzzle ();
                 count := 0;
                 solve ();
                 processArgs rest)
            else processArgs rest
    in processArgs args;
       let val stop = Time.now ()
           val diff = Time.toReal (Time.- (stop, start))
       in print ("Seconds to process " ^ Real.fmt (StringCvt.FIX (SOME 3)) diff ^ "\n") end
    end

end

val _ = Sudoku.main ()
