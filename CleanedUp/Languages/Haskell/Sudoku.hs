import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (transpose, nub, (\\))
import Data.Char (digitToInt, isDigit)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

type Board = [[Int]]

main :: IO ()
main = do
    start <- getCurrentTime
    args <- getArgs
    print args
    mapM_ processFile (filter (\f -> reverse (take 7 (reverse f)) == ".matrix") args)
    end <- getCurrentTime
    printf "Seconds to process %.3f\n" (realToFrac (diffUTCTime end start) :: Double)

processFile :: String -> IO ()
processFile filename = do
    putStrLn filename
    content <- readFile filename
    let board = parseBoard content
    printBoard board
    let (solved, solution, count) = solve board 0
    if solved
        then do
            printBoard solution
            putStrLn $ "Solved in Iterations=" ++ show count
        else putStrLn "No solution found"

parseBoard :: String -> Board
parseBoard content = 
    let validLines = filter (\l -> not (null l) && head l /= '#') (lines content)
        rows = take 9 validLines
    in map (map (\w -> read w :: Int) . words) rows

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "\nPuzzle:"
    mapM_ (putStrLn . unwords . map show) board

-- Explicit recursion to count iterations
solve :: Board -> Int -> (Bool, Board, Int)
solve board count = 
    case findEmpty board of
        Nothing -> (True, board, count)
        Just (r, c) -> tryValues board r c 1 count

tryValues :: Board -> Int -> Int -> Int -> Int -> (Bool, Board, Int)
tryValues board r c val count
    | val > 9 = (False, board, count)
    | otherwise = 
        let count' = count + 1
        in if isValid board r c val
           then 
               let board' = update board r c val
                   (solved, resBoard, finalCount) = solve board' count'
               in if solved 
                  then (True, resBoard, finalCount)
                  else tryValues board r c (val + 1) finalCount
           else tryValues board r c (val + 1) count'

findEmpty :: Board -> Maybe (Int, Int)
findEmpty board = 
    let empties = [(r, c) | r <- [0..8], c <- [0..8], (board !! r) !! c == 0]
    in if null empties then Nothing else Just (head empties)

isValid :: Board -> Int -> Int -> Int -> Bool
isValid board r c val =
    val `notElem` (board !! r) &&
    val `notElem` (transpose board !! c) &&
    val `notElem` getBox board r c

getBox :: Board -> Int -> Int -> [Int]
getBox board r c =
    let r0 = (r `div` 3) * 3
        c0 = (c `div` 3) * 3
    in [ (board !! i) !! j | i <- [r0..r0+2], j <- [c0..c0+2] ]

update :: Board -> Int -> Int -> Int -> Board
update board r c val =
    take r board ++
    [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++
    drop (r + 1) board
