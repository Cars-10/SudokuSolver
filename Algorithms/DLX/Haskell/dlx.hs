{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad.ST
import Control.Monad (when, forM_, foldM)
import Control.Monad.Fix (mfix)
import Data.STRef
import Data.Array.ST
import Data.Array (Array, (!), listArray, bounds, elems)
import Text.Printf (printf)

-- DLX Node structure
data DNode s = DNode
  { left :: !(STRef s (DNode s))
  , right :: !(STRef s (DNode s))
  , up :: !(STRef s (DNode s))
  , down :: !(STRef s (DNode s))
  , column :: !(STRef s (DNode s))
  , size :: !(STRef s Int)
  , rowId :: !Int
  , colId :: !Int
  }

-- Compare nodes by their IDs (since STRefs can't be compared directly)
instance Eq (DNode s) where
  n1 == n2 = rowId n1 == rowId n2 && colId n1 == colId n2

-- Parse matrix file
parseMatrix :: String -> [[Int]]
parseMatrix content =
    let validLines = filter (\l -> not (null l) && not (null l) && head l /= '#') (lines content)
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

    let result = runST $ solveDLX board
    case result of
        Just (solution, iters) -> do
            printBoard solution
            printf "\nSolved in Iterations=%d\n\n" iters
        Nothing -> putStrLn "No solution found"

-- Solve Sudoku using DLX
solveDLX :: [[Int]] -> ST s (Maybe ([[Int]], Int))
solveDLX puzzle = do
    -- Build DLX matrix
    (root, rowInfos, clueRows) <- buildDLXMatrix puzzle

    -- Cover clue rows before search (critical for correct iteration count!)
    coverClues clueRows

    -- Create iteration counter
    iterRef <- newSTRef 0

    -- Create solution list (stores row IDs)
    solutionRef <- newSTRef []

    -- Run search
    found <- search root iterRef solutionRef

    if found
        then do
            rowIds <- readSTRef solutionRef
            iters <- readSTRef iterRef
            let solution = reconstructSolution puzzle rowIds rowInfos
            return $ Just (solution, iters)
        else return Nothing

-- Cover all clue rows before search starts
coverClues :: [DNode s] -> ST s ()
coverClues clueRows = mapM_ coverClueRow clueRows

-- Cover all columns in a clue row
coverClueRow :: DNode s -> ST s ()
coverClueRow firstNode = do
    -- Cover all columns in this row (traverse circularly)
    let coverLoop node start = do
            col <- readSTRef (column node)
            cover col
            nextNode <- readSTRef (right node)
            when (nextNode /= start) $ coverLoop nextNode start
    coverLoop firstNode firstNode

-- Build DLX matrix for Sudoku
-- Returns: (root, rowInfos, clueRows) where clueRows is list of (first node of row) for clue cells
buildDLXMatrix :: [[Int]] -> ST s (DNode s, [(Int, Int, Int)], [DNode s])
buildDLXMatrix puzzle = do
    -- Create root node
    root <- newDNode (-1) (-1)

    -- Create column headers (324 columns for Sudoku constraints)
    columns <- sequence [newDNode (-1) i | i <- [0..323]]

    -- Link columns into circular list with root
    linkColumns root columns

    -- Build rows for each cell and possible value
    (rowInfos, clueRows) <- buildRows puzzle columns

    return (root, rowInfos, clueRows)

-- Create a new DLX node using mfix for circular reference
newDNode :: Int -> Int -> ST s (DNode s)
newDNode rid cid = mfix $ \node -> do
    l <- newSTRef node
    r <- newSTRef node
    u <- newSTRef node
    d <- newSTRef node
    c <- newSTRef node
    s <- newSTRef 0
    return $ DNode l r u d c s rid cid

-- Link column headers into circular list with root
linkColumns :: DNode s -> [DNode s] -> ST s ()
linkColumns root cols = do
    -- Link all columns left-to-right in circular list
    let linkPair node1 node2 = do
            writeSTRef (right node1) node2
            writeSTRef (left node2) node1

    -- Link root -> col0 -> col1 -> ... -> col323 -> root
    case cols of
        [] -> writeSTRef (right root) root >> writeSTRef (left root) root
        (c:cs) -> do
            linkPair root c
            linkChain c cs
            case reverse cols of
                (lastCol:_) -> linkPair lastCol root
                [] -> return ()

linkChain :: DNode s -> [DNode s] -> ST s ()
linkChain _ [] = return ()
linkChain prev (curr:rest) = do
    writeSTRef (right prev) curr
    writeSTRef (left curr) prev
    linkChain curr rest

-- Build constraint rows
-- Returns: (rowInfos, clueRows) where clueRows contains first node of each clue row
buildRows :: [[Int]] -> [DNode s] -> ST s ([(Int, Int, Int)], [DNode s])
buildRows puzzle columns = do
    let buildCell r c = do
            let cellVal = (puzzle !! r) !! c
            if cellVal == 0
                then do
                    results <- sequence [addRow r c d columns | d <- [1..9]]
                    return (map fst results, [])  -- No clue rows for empty cells
                else do
                    (info, firstNode) <- addRow r c cellVal columns
                    return ([info], [firstNode])  -- This is a clue row

    results <- sequence [buildCell r c | r <- [0..8], c <- [0..8]]
    let (infoLists, clueLists) = unzip results
    return (concat infoLists, concat clueLists)

-- Add a single row to DLX matrix
-- Returns: ((r, c, d), firstNode) where firstNode is the first node in the row
addRow :: Int -> Int -> Int -> [DNode s] -> ST s ((Int, Int, Int), DNode s)
addRow r c d columns = do
    -- Calculate column indices (0-indexed, matching C implementation)
    let posCol = r * 9 + c
    let rowCol = 81 + r * 9 + (d - 1)
    let colCol = 162 + c * 9 + (d - 1)
    let boxCol = 243 + ((r `div` 3) * 3 + (c `div` 3)) * 9 + (d - 1)

    -- Create 4 nodes for this row
    let rid = r * 81 + c * 9 + d
    nodes <- sequence [newDNode rid i | i <- [posCol, rowCol, colCol, boxCol]]

    -- Link nodes in circular list (row-wise)
    linkNodesRow nodes

    -- Add each node to its column
    mapM_ (\(node, colIdx) -> addNodeToColumn node (columns !! colIdx)) (zip nodes [posCol, rowCol, colCol, boxCol])

    return ((r, c, d), head nodes)

-- Link nodes in a row into circular list
linkNodesRow :: [DNode s] -> ST s ()
linkNodesRow [] = return ()
linkNodesRow [single] = writeSTRef (right single) single >> writeSTRef (left single) single
linkNodesRow nodes = do
    let linkPair n1 n2 = writeSTRef (right n1) n2 >> writeSTRef (left n2) n1
    -- Link: n0 <-> n1 <-> n2 <-> n3 <-> n0
    sequence_ [linkPair (nodes !! i) (nodes !! ((i + 1) `mod` length nodes)) | i <- [0..length nodes - 1]]

-- Add node to column's circular vertical list
addNodeToColumn :: DNode s -> DNode s -> ST s ()
addNodeToColumn node col = do
    -- Set node's column pointer
    writeSTRef (column node) col

    -- Insert node at end of column (before column header)
    colUp <- readSTRef (up col)
    writeSTRef (down colUp) node
    writeSTRef (up node) colUp
    writeSTRef (down node) col
    writeSTRef (up col) node

    -- Increment column size
    modifySTRef (size col) (+1)

-- Cover a column
cover :: DNode s -> ST s ()
cover col = do
    -- Remove column header from header list
    colL <- readSTRef (left col)
    colR <- readSTRef (right col)
    writeSTRef (right colL) colR
    writeSTRef (left colR) colL

    -- Remove all rows in this column
    let coverRow rowNode = do
            -- Remove all nodes in this row from their columns
            let removeNode node = do
                    nCol <- readSTRef (column node)
                    nUp <- readSTRef (up node)
                    nDown <- readSTRef (down node)
                    writeSTRef (down nUp) nDown
                    writeSTRef (up nDown) nUp
                    modifySTRef (size nCol) (subtract 1)

            let traverseRow n start = do
                    removeNode n
                    nRight <- readSTRef (right n)
                    when (nRight /= start) $ traverseRow nRight start

            rowRight <- readSTRef (right rowNode)
            when (rowRight /= rowNode) $ traverseRow rowRight rowNode

    let traverseCol n start = do
            when (n /= start) $ do
                coverRow n
                nDown <- readSTRef (down n)
                traverseCol nDown start

    colDown <- readSTRef (down col)
    traverseCol colDown col

-- Uncover a column (exact reverse of cover)
uncover :: DNode s -> ST s ()
uncover col = do
    -- Restore all rows in this column (in reverse order)
    let uncoverRow rowNode = do
            -- Restore all nodes in this row
            let restoreNode node = do
                    nCol <- readSTRef (column node)
                    nUp <- readSTRef (up node)
                    nDown <- readSTRef (down node)
                    writeSTRef (down nUp) node
                    writeSTRef (up nDown) node
                    modifySTRef (size nCol) (+1)

            let traverseRow n start = do
                    nLeft <- readSTRef (left n)
                    when (nLeft /= start) $ traverseRow nLeft start
                    restoreNode n

            rowLeft <- readSTRef (left rowNode)
            when (rowLeft /= rowNode) $ traverseRow rowLeft rowNode

    let traverseCol n start = do
            nUp <- readSTRef (up n)
            when (nUp /= start) $ traverseCol nUp start
            when (n /= start) $ uncoverRow n

    colUp <- readSTRef (up col)
    traverseCol colUp col

    -- Restore column header to header list
    colL <- readSTRef (left col)
    colR <- readSTRef (right col)
    writeSTRef (right colL) col
    writeSTRef (left colR) col

-- Choose column with minimum size
chooseColumn :: DNode s -> ST s (Maybe (DNode s))
chooseColumn root = do
    rootR <- readSTRef (right root)
    if rootR == root
        then return Nothing
        else do
            let findMin currentCol bestCol bestSize = do
                    currR <- readSTRef (right currentCol)
                    if currR == root
                        then return (Just bestCol)
                        else do
                            currSize <- readSTRef (size currentCol)
                            if currSize < bestSize
                                then findMin currR currentCol currSize
                                else findMin currR bestCol bestSize

            firstSize <- readSTRef (size rootR)
            findMin rootR rootR firstSize

-- DLX search (Algorithm X)
search :: DNode s -> STRef s Int -> STRef s [Int] -> ST s Bool
search root iterRef solutionRef = do
    -- Increment iteration counter
    modifySTRef iterRef (+1)

    -- Check if matrix is empty (solution found)
    rootR <- readSTRef (right root)
    if rootR == root
        then return True
        else do
            -- Choose column with minimum size
            maybeCol <- chooseColumn root
            case maybeCol of
                Nothing -> return False
                Just col -> do
                    colSize <- readSTRef (size col)
                    if colSize == 0
                        then return False
                        else do
                            -- Cover this column
                            cover col

                            -- Try each row in this column
                            let tryRow rowNode = do
                                    -- Add row to solution
                                    let rid = rowId rowNode
                                    modifySTRef solutionRef (rid :)

                                    -- Cover all other columns in this row
                                    let coverRowCols n start = do
                                            nCol <- readSTRef (column n)
                                            cover nCol
                                            nRight <- readSTRef (right n)
                                            when (nRight /= start) $ coverRowCols nRight start

                                    rowRight <- readSTRef (right rowNode)
                                    when (rowRight /= rowNode) $ coverRowCols rowRight rowNode

                                    -- Recurse
                                    found <- search root iterRef solutionRef

                                    if found
                                        then return True
                                        else do
                                            -- Backtrack: uncover columns in reverse
                                            let uncoverRowCols n start = do
                                                    nLeft <- readSTRef (left n)
                                                    when (nLeft /= start) $ uncoverRowCols nLeft start
                                                    nCol <- readSTRef (column n)
                                                    uncover nCol

                                            rowLeft <- readSTRef (left rowNode)
                                            when (rowLeft /= rowNode) $ uncoverRowCols rowLeft rowNode

                                            -- Remove row from solution
                                            modifySTRef solutionRef tail

                                            -- Try next row
                                            return False

                            let tryAllRows currentRow = do
                                    colNode <- readSTRef (down col)  -- Start from first row in column
                                    let loop n = do
                                            if n == col
                                                then return False
                                                else do
                                                    found <- tryRow n
                                                    if found
                                                        then return True
                                                        else do
                                                            nDown <- readSTRef (down n)
                                                            loop nDown
                                    loop colNode

                            found <- tryAllRows col
                            uncover col
                            return found

-- Reconstruct solution from row IDs
reconstructSolution :: [[Int]] -> [Int] -> [(Int, Int, Int)] -> [[Int]]
reconstructSolution puzzle rowIds rowInfos =
    let solution = [[puzzle !! r !! c | c <- [0..8]] | r <- [0..8]]
        -- Decode row IDs to (row, col, digit)
        decodeRow rid = (rid `div` 81, (rid `mod` 81) `div` 9, rid - (rid `div` 81) * 81 - ((rid `mod` 81) `div` 9) * 9)
        updates = map decodeRow rowIds
        applyUpdate grid (r, c, d) = take r grid ++ [take c (grid !! r) ++ [d] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid
    in foldl applyUpdate solution updates
