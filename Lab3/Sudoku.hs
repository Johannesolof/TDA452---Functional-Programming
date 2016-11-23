module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = validRows && validCols && validCells
    where validRows   = length (rows s) == 9
          validCols   = all (\col -> length col == 9) (rows s)
          validCells  = and $ concatMap (map validCell) (rows s)
          validCell c = isNothing c || (c > Just 0 && c < Just 10)

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [ and [ isJust n | n <- row] | row <- rows s]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = do
  putStrLn $ formatSudoku s
  return ()

formatSudoku :: Sudoku -> String
formatSudoku s = unlines $ map (map maybeToChar) (rows s)

maybeToChar :: Maybe Int -> Char
maybeToChar (Just n)  = chr (n+offset)
maybeToChar Nothing   = '.'

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                file <- readFile fp
                return $ parseSudoku file

parseSudoku :: String -> Sudoku
parseSudoku str =  Sudoku $ map (map charToMaybe) $ lines str


charToMaybe :: Char -> Maybe Int
charToMaybe c | n > min && n < max = Just (n - offset)
              | n == dot           = Nothing
              | otherwise          = error
                                     ("Not a valid Sudoku character: " ++ [c])
  where n = ord c
        min = offset
        max = offset + 10
        dot = offset - 2

offset :: Int
offset = ord '0'

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, genNumber), (9, genNothing)]

genNothing :: Gen (Maybe Int)
genNothing = return Nothing

genNumber :: Gen (Maybe Int)
genNumber = elements [Just n|n<-[1..9]]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock $ blocks s

isOkayBlock :: Block -> Bool
isOkayBlock b = length fb == length (nub fb)
  where fb = filter isJust b

blocks :: Sudoku -> [Block]
blocks s = concat [rows s, cols, squares]
  where cols    = transpose $ rows s
        squares = parseSquares s

parseSquares :: Sudoku -> [Block]
parseSquares s = [concatMap snd $
                  filter (\x -> fst x == i) zipped | i <- [0..8]]
  where zipped = zip indices splitSudoku
        indices = [ i `mod` 3 + 3 * (i `div` 9) | i <- [0..26]]
        splitSudoku = chunksOf 3 $ concat $ rows s



-------------------------------------------------------------------------

example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: String
example2 =  "36..712..\n" ++
            ".5....18.\n" ++
            "..92.47..\n" ++
            "....13.28\n" ++
            "4..5.2..9\n" ++
            "27.46....\n" ++
            "..53.89..\n" ++
            ".83....6.\n" ++
            "..769..43"
