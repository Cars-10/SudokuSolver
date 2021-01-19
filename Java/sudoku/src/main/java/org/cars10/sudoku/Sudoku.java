package org.cars10.sudoku;

import java.io.IOException;
import org.apache.commons.io.FilenameUtils;
import java.util.*;
import java.io.BufferedReader;
import java.io.FileReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Sudoku {
    static final Logger logger = LogManager.getLogger(Sudoku.class.getName());
    static int count;
    static int[][] puzzle;

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
                }
            }
        } catch (Exception e) {
            logger.info("Exception {}", e.toString());
        }
    } 

    public static void printPuzzle() {
        logger.info("%nPuzzle:%n");
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                logger.info("%i ", puzzle[j][i]);
            }
            logger.info("%n");
        }
    }

    public static void main(String[] args) {
        long start = System.currentTimeMillis();

        // For each .matrix file supplied on the commandline run the solver
        for(int i=0;i<args.length;i++) { 
            if (FilenameUtils.getExtension(args[i]).equals(".matrix")) {
                logger.info(args[i]);  
                try {
                    readMatrixFile(args[i]);
                } catch (IOException ex) {
                    logger.info("File cannot be opened for reading {}%n", ex.toString());  
                    System.exit(0); 
                }
                printPuzzle(); 
                count = 0;
                //r = solve();
            }
        }

        long stop = System.currentTimeMillis();
        logger.info("Seconds to process %.3f%n", (stop - start)/1000); 
        System.exit(0); 
    }
}