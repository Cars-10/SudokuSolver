package org.cars10.sudoku;

import java.io.IOException;
import org.apache.commons.io.FilenameUtils;
import java.util.*;
import java.io.BufferedReader;
import java.io.FileReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;

public class Sudoku {
    private static Logger logger = LogManager.getLogger(org.cars10.sudoku.Sudoku.class.getName());
    static int count;
    static int[][] puzzle = new int[9][9];

    public static void readMatrixFile(String filename) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int j = 0;
            while ((line = br.readLine()) != null) {
                if (!line.substring(0,1).equals("#")) {
                Scanner sc = new Scanner(line);
                    for (int i = 0; i < 9; i++) {
                        puzzle[j][i] = sc.nextInt();
                    }
                sc.close();
                j++;
                }
            }
        } catch (Exception e) {
            logger.error("Exception {}", e.toString());
        }
    } 

    public static void printPuzzle() {
        StringBuilder bld = new StringBuilder();
        logger.info("\nPuzzle:");

        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                logger.debug("puzzle[{}][{}]={} ",j,i,puzzle[j][i]);
                bld.append(" " + puzzle[j][i]);
            }
        String line = bld.toString();
        bld.setLength(0);
        logger.info("{} ", line);
        }
    }
    
    public static int isPossible(int x, int y, int val) { 
        logger.debug("Is possible {}, {}, {} count={}" ,x ,y ,val,count);
        // Find if a matching number (val) already exists
        // in the same row (y) or column (x) or within its rectangle
        for (int i = 0; i <9; i++) if(puzzle[i][x] == val) return 0; 
        for (int i = 0; i <9; i++) if(puzzle[y][i] == val) return 0; 
    
        // Search the Rectangle containing x & y
        // Find which 3x3 square we are in using the floor quotient
        int x0= (Math.floorDiv(x,3))*3;
        int y0= (Math.floorDiv(y,3))*3;
        logger.debug("Is possible x={} x0={}, y={} y0={}, val={}" , x, x0, y, y0, val);

        for (int i = 0; i <3; i++)
        {
            for (int j = 0; j <3; j++)
            {
                logger.debug("y0+i={} i={}, x0+j={} j={} Puzzle[y0+i][x0+j]={}, val={}", y0+i,i, x0+j,j, puzzle[y0+i][x0+j] , val);
                if(puzzle[y0+i][x0+j] == val ) return 0; 
            }
        }
        logger.debug("YES possible {}, {}, {}" ,x ,y ,val);
        return 1;
    }

    public static int solve() {
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                logger.debug("i={},j={}: {}",i,j,puzzle[j][i]);
                if (puzzle[j][i] == 0) {
                    for (int val = 1; val < 10; val++) {
                        count += 1;
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
        logger.info("\nSolved in Iterations={}", count);
        return 2;
    }

    public static void main(String[] args) {
        long start = System.currentTimeMillis();
        String numberAsString;
        
        // For each .matrix file supplied on the commandline run the solver
        for(int i=0;i<args.length;i++) { 
            if (FilenameUtils.getExtension(args[i]).equals("matrix")) {
                logger.info(args[i]);  
                try {
                    readMatrixFile(args[i]);
                } catch (IOException ex) {
                    logger.error("File cannot be opened for reading {}", ex.toString());  
                    System.exit(0); 
                }
                printPuzzle(); 
                count = 0;
                solve();
            }
        }

        numberAsString = String.format("%.3f", (System.currentTimeMillis() - start)/1000.0);
        logger.info("Seconds to process {}", numberAsString);  
        System.exit(0); 
    }
}