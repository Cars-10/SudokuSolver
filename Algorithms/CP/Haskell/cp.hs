{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad.ST
import Control.Monad (when, forM, forM_)
import Data.STRef
import Data.Array.ST
import Data.Bits
import Text.Printf (printf)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Type aliases
type Grid s = STUArray s (Int, Int) Int
type Candidates s = STUArray s (Int, Int) Int

-- Parse matrix file
parseMatrix :: String -> [[Int]]
parseMatrix content =
    let validLines = filter (\l -> not (null l) && head l /= '#') (lines content)
        rows = take 9 validLines
    in map (map (\w -> read w :: Int) . words) rows

-- Print board formatted
printBoard :: [[Int]] -> IO ()
printBoard board = do
    putStrLn "\nPuzzle:"
    mapM_ (\row -> putStrLn $ unwords (map show row) ++ " ") board

-- Main entry point
main :: IO ()
main = do
    args <- getArgs
    let matrixFiles = filter (\f -> reverse (take 7 (reverse f)) == ".matrix") args
    mapM_ solveFile matrixFiles

solveFile :: String -> IO ()
solveFile filename = do
    putStrLn filename
    content <- readFile filename
    let board = parseMatrix content
    -- Print raw board
    mapM_ (\row -> putStrLn $ unwords (map show row) ++ " ") board
    printBoard board

    let result = runST $ solveCP board
    case result of
        Just (solution, iters) -> do
            printBoard solution
            printf "\nSolved in Iterations=%d\n\n" iters
        Nothing -> putStrLn "No solution found"

-- Solve Sudoku using Constraint Propagation
solveCP :: [[Int]] -> ST s (Maybe ([[Int]], Int))
solveCP puzzle = do
    -- Create mutable grid and candidates arrays
    grid <- newArray ((0,0), (8,8)) 0 :: ST s (STUArray s (Int, Int) Int)
    candidates <- newArray ((0,0), (8,8)) 0 :: ST s (STUArray s (Int, Int) Int)

    -- Create iteration counter
    iterRef <- newSTRef 0

    -- Initialize grid and candidates
    initGrid grid candidates puzzle iterRef

    -- Propagate initial constraints
    success <- propagate grid candidates iterRef

    if not success
        then return Nothing
        else do
            -- Run search
            found <- cpSearch grid candidates iterRef

            if found
                then do
                    solution <- extractSolution grid
                    iters <- readSTRef iterRef
                    return $ Just (solution, iters)
                else return Nothing

-- Initialize grid and candidates
initGrid :: Grid s -> Candidates s -> [[Int]] -> STRef s Int -> ST s ()
initGrid grid candidates puzzle iterRef = do
    forM_ [0..8] $ \r -> do
        forM_ [0..8] $ \c -> do
            let val = (puzzle !! r) !! c
            if val == 0
                then do
                    -- Empty cell: all candidates 1-9
                    writeArray grid (r, c) 0
                    writeArray candidates (r, c) 0x3FE  -- bits 1-9 set
                else do
                    -- Given clue
                    writeArray grid (r, c) val
                    writeArray candidates (r, c) (shiftL 1 val)

-- Get all peers for a cell (20 cells: 8 in row + 8 in col + 4 in box)
getPeers :: Int -> Int -> [(Int, Int)]
getPeers row col =
    let rowPeers = [(row, c) | c <- [0..8], c /= col]
        colPeers = [(r, col) | r <- [0..8], r /= row]
        boxRow = (row `div` 3) * 3
        boxCol = (col `div` 3) * 3
        boxPeers = [(r, c) | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2], r /= row, c /= col]
    in rowPeers ++ colPeers ++ boxPeers

-- Check if digit is a candidate
hasCandidate :: Int -> Int -> Bool
hasCandidate cands digit = testBit cands digit

-- Remove digit from candidates
removeCandidate :: Int -> Int -> Int
removeCandidate cands digit = clearBit cands digit

-- Count number of candidates
countCandidates :: Int -> Int
countCandidates cands = popCount cands

-- Get first candidate digit
getFirstCandidate :: Int -> Maybe Int
getFirstCandidate cands =
    case [d | d <- [1..9], testBit cands d] of
        [] -> Nothing
        (d:_) -> Just d

-- Eliminate a digit from a cell
eliminate :: Grid s -> Candidates s -> STRef s Int -> Int -> Int -> Int -> ST s Bool
eliminate grid candidates iterRef row col digit = do
    cands <- readArray candidates (row, col)

    -- Already eliminated
    if not (hasCandidate cands digit)
        then return True
        else do
            -- Remove digit
            let newCands = removeCandidate cands digit
            writeArray candidates (row, col) newCands

            -- Check for contradiction
            let remaining = countCandidates newCands
            if remaining == 0
                then return False
                else if remaining == 1
                    then do
                        -- Only one candidate left - assign it
                        val <- readArray grid (row, col)
                        if val == 0
                            then case getFirstCandidate newCands of
                                Just lastDigit -> assign grid candidates iterRef row col lastDigit
                                Nothing -> return False
                            else return True
                    else return True

-- Assign a digit to a cell
assign :: Grid s -> Candidates s -> STRef s Int -> Int -> Int -> Int -> ST s Bool
assign grid candidates iterRef row col digit = do
    -- Check if already assigned (avoid double-counting)
    currentVal <- readArray grid (row, col)
    if currentVal /= 0
        then return (currentVal == digit)  -- OK if same digit, contradiction if different
        else do
            -- Increment iteration counter
            modifySTRef iterRef (+1)

            -- Set value
            writeArray grid (row, col) digit
            writeArray candidates (row, col) (shiftL 1 digit)

            -- Eliminate digit from all peers
            let peers = getPeers row col
            let eliminateFromPeer (pr, pc) = eliminate grid candidates iterRef pr pc digit

            results <- mapM eliminateFromPeer peers
            return $ and results

-- Propagate constraints (incremental like C reference)
propagate :: Grid s -> Candidates s -> STRef s Int -> ST s Bool
propagate grid candidates iterRef = propagateLoop
  where
    propagateLoop = do
        -- Strategy 1: Singleton elimination (process all, track changes)
        (success1, changed1) <- processSingletons
        if not success1
            then return False
            else do
                -- Strategy 2: Hidden singles in rows
                (success2, changed2) <- processHiddenSinglesRows
                if not success2
                    then return False
                    else do
                        -- Strategy 3: Hidden singles in columns
                        (success3, changed3) <- processHiddenSinglesCols
                        if not success3
                            then return False
                            else do
                                -- Strategy 4: Hidden singles in boxes
                                (success4, changed4) <- processHiddenSinglesBoxes
                                if not success4
                                    then return False
                                    else do
                                        let anyChanged = changed1 || changed2 || changed3 || changed4
                                        if anyChanged
                                            then propagateLoop
                                            else return True

    -- Process singletons incrementally
    processSingletons = processCells 0 0 False
      where
        processCells r c changed
            | r >= 9 = return (True, changed)
            | c >= 9 = processCells (r + 1) 0 changed
            | otherwise = do
                val <- readArray grid (r, c)
                if val == 0
                    then do
                        cands <- readArray candidates (r, c)
                        let numCands = countCandidates cands
                        if numCands == 0
                            then return (False, changed)  -- Contradiction
                            else if numCands == 1
                                then case getFirstCandidate cands of
                                    Just digit -> do
                                        success <- assign grid candidates iterRef r c digit
                                        if success
                                            then processCells r (c + 1) True
                                            else return (False, changed)
                                    Nothing -> return (False, changed)
                                else processCells r (c + 1) changed
                    else processCells r (c + 1) changed

    -- Process hidden singles in rows incrementally
    processHiddenSinglesRows = processRows 0 False
      where
        processRows r changed
            | r >= 9 = return (True, changed)
            | otherwise = do
                (success, rowChanged) <- processDigits r 1 False
                if success
                    then processRows (r + 1) (changed || rowChanged)
                    else return (False, changed)

        processDigits r digit changed
            | digit > 9 = return (True, changed)
            | otherwise = do
                -- Check if digit already assigned in row
                rowVals <- mapM (\c -> readArray grid (r, c)) [0..8]
                if digit `elem` rowVals
                    then processDigits r (digit + 1) changed
                    else do
                        -- Count cells where digit is candidate
                        (count, lastCol) <- countInRow r digit 0 0 (-1)
                        if count == 1
                            then do
                                success <- assign grid candidates iterRef r lastCol digit
                                if success
                                    then processDigits r (digit + 1) True
                                    else return (False, changed)
                            else if count == 0
                                then return (False, changed)  -- Contradiction
                                else processDigits r (digit + 1) changed

        countInRow r digit c count lastCol
            | c >= 9 = return (count, lastCol)
            | otherwise = do
                cands <- readArray candidates (r, c)
                if hasCandidate cands digit
                    then countInRow r digit (c + 1) (count + 1) c
                    else countInRow r digit (c + 1) count lastCol

    -- Process hidden singles in columns incrementally
    processHiddenSinglesCols = processCols 0 False
      where
        processCols c changed
            | c >= 9 = return (True, changed)
            | otherwise = do
                (success, colChanged) <- processDigits c 1 False
                if success
                    then processCols (c + 1) (changed || colChanged)
                    else return (False, changed)

        processDigits c digit changed
            | digit > 9 = return (True, changed)
            | otherwise = do
                -- Check if digit already assigned in column
                colVals <- mapM (\r -> readArray grid (r, c)) [0..8]
                if digit `elem` colVals
                    then processDigits c (digit + 1) changed
                    else do
                        -- Count cells where digit is candidate
                        (count, lastRow) <- countInCol c digit 0 0 (-1)
                        if count == 1
                            then do
                                success <- assign grid candidates iterRef lastRow c digit
                                if success
                                    then processDigits c (digit + 1) True
                                    else return (False, changed)
                            else if count == 0
                                then return (False, changed)  -- Contradiction
                                else processDigits c (digit + 1) changed

        countInCol c digit r count lastRow
            | r >= 9 = return (count, lastRow)
            | otherwise = do
                cands <- readArray candidates (r, c)
                if hasCandidate cands digit
                    then countInCol c digit (r + 1) (count + 1) r
                    else countInCol c digit (r + 1) count lastRow

    -- Process hidden singles in boxes incrementally
    processHiddenSinglesBoxes = processBoxes 0 False
      where
        processBoxes box changed
            | box >= 9 = return (True, changed)
            | otherwise = do
                let boxRow = (box `div` 3) * 3
                let boxCol = (box `mod` 3) * 3
                (success, boxChanged) <- processDigits boxRow boxCol 1 False
                if success
                    then processBoxes (box + 1) (changed || boxChanged)
                    else return (False, changed)

        processDigits boxRow boxCol digit changed
            | digit > 9 = return (True, changed)
            | otherwise = do
                -- Check if digit already assigned in box
                boxVals <- sequence [readArray grid (r, c) | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]
                if digit `elem` boxVals
                    then processDigits boxRow boxCol (digit + 1) changed
                    else do
                        -- Count cells where digit is candidate
                        (count, lastR, lastC) <- countInBox boxRow boxCol digit 0 0 0 (-1) (-1)
                        if count == 1
                            then do
                                success <- assign grid candidates iterRef lastR lastC digit
                                if success
                                    then processDigits boxRow boxCol (digit + 1) True
                                    else return (False, changed)
                            else if count == 0
                                then return (False, changed)  -- Contradiction
                                else processDigits boxRow boxCol (digit + 1) changed

        countInBox boxRow boxCol digit dr dc count lastR lastC
            | dr >= 3 = return (count, lastR, lastC)
            | dc >= 3 = countInBox boxRow boxCol digit (dr + 1) 0 count lastR lastC
            | otherwise = do
                let r = boxRow + dr
                let c = boxCol + dc
                cands <- readArray candidates (r, c)
                if hasCandidate cands digit
                    then countInBox boxRow boxCol digit dr (dc + 1) (count + 1) r c
                    else countInBox boxRow boxCol digit dr (dc + 1) count lastR lastC

-- Find MRV cell (cell with minimum remaining values)
findMRVCell :: Grid s -> Candidates s -> ST s (Maybe (Int, Int))
findMRVCell grid candidates = do
    cells <- forM [0..8] $ \r -> do
        forM [0..8] $ \c -> do
            val <- readArray grid (r, c)
            if val == 0
                then do
                    cands <- readArray candidates (r, c)
                    let numCands = countCandidates cands
                    return [(r, c, numCands)]
                else return []
    let emptyCells = concat $ concat cells
    if null emptyCells
        then return Nothing
        else do
            let (r, c, _) = minimumBy (comparing (\(_,_,n) -> n)) emptyCells
            return $ Just (r, c)

-- CP search with backtracking
cpSearch :: Grid s -> Candidates s -> STRef s Int -> ST s Bool
cpSearch grid candidates iterRef = do
    -- Find MRV cell
    maybeCell <- findMRVCell grid candidates
    case maybeCell of
        Nothing -> return True  -- No empty cells - solved!
        Just (r, c) -> do
            -- Get candidates for this cell
            cands <- readArray candidates (r, c)
            let digits = [d | d <- [1..9], hasCandidate cands d]

            -- Try each candidate
            tryDigits digits r c

  where
    tryDigits [] _ _ = return False
    tryDigits (d:ds) r c = do
        -- Save state
        gridCopy <- copyGrid grid
        candsCopy <- copyCandidates candidates

        -- Try assigning digit
        success <- assign grid candidates iterRef r c d
        if success
            then do
                -- Propagate and recurse
                propSuccess <- propagate grid candidates iterRef
                if propSuccess
                    then do
                        found <- cpSearch grid candidates iterRef
                        if found
                            then return True
                            else do
                                -- Backtrack
                                restoreGrid grid gridCopy
                                restoreCandidates candidates candsCopy
                                tryDigits ds r c
                    else do
                        -- Backtrack
                        restoreGrid grid gridCopy
                        restoreCandidates candidates candsCopy
                        tryDigits ds r c
            else do
                -- Backtrack
                restoreGrid grid gridCopy
                restoreCandidates candidates candsCopy
                tryDigits ds r c

-- Copy grid
copyGrid :: Grid s -> ST s [[Int]]
copyGrid grid = do
    rows <- forM [0..8] $ \r -> do
        forM [0..8] $ \c -> do
            readArray grid (r, c)
    return rows

-- Copy candidates
copyCandidates :: Candidates s -> ST s [[Int]]
copyCandidates cands = do
    rows <- forM [0..8] $ \r -> do
        forM [0..8] $ \c -> do
            readArray cands (r, c)
    return rows

-- Restore grid
restoreGrid :: Grid s -> [[Int]] -> ST s ()
restoreGrid grid gridCopy = do
    forM_ [0..8] $ \r -> do
        forM_ [0..8] $ \c -> do
            writeArray grid (r, c) ((gridCopy !! r) !! c)

-- Restore candidates
restoreCandidates :: Candidates s -> [[Int]] -> ST s ()
restoreCandidates cands candsCopy = do
    forM_ [0..8] $ \r -> do
        forM_ [0..8] $ \c -> do
            writeArray cands (r, c) ((candsCopy !! r) !! c)

-- Extract solution from grid
extractSolution :: Grid s -> ST s [[Int]]
extractSolution grid = do
    forM [0..8] $ \r -> do
        forM [0..8] $ \c -> do
            readArray grid (r, c)
