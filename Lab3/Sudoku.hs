module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe

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
maybeToChar (Just n)  = chr (n+48)
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
charToMaybe c | n > 48 && n < 58 = Just (n - 48)
              | n == 46          = Nothing
              | otherwise        = error 
                                     ("Not a valid Sudoku character: " ++ [c])
  where n = ord c

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

isOkayBlock :: Block -> Bool
isOkayBlock b = length b == length (fromList b)

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
