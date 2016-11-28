module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split


-------------------------------------------------------------------------
-- * Assignment A

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
-- * -- * Assignment B

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = do
  putStrLn $ formatSudoku s
  return ()

-- Formats Sudoku data type into a string representation
formatSudoku :: Sudoku -> String
formatSudoku s = unlines $ map (map maybeToChar) (rows s)

-- Formats Maybe Int into char representation
maybeToChar :: Maybe Int -> Char
maybeToChar (Just n)  = chr (n+offset)
maybeToChar Nothing   = '.'

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                file <- readFile fp
                return $ parseSudoku file

-- Parses data from a string into Sudoku data type
parseSudoku :: String -> Sudoku
parseSudoku str =  Sudoku $ map (map charToMaybe) $ lines str

-- Converts a character into an Maybe integer
charToMaybe :: Char -> Maybe Int
charToMaybe c | n > min && n < max = Just (n - offset)
              | n == dot           = Nothing
              | otherwise          = error
                                     ("Not a valid Sudoku character: " ++ [c])
  where n = ord c
        min = offset
        max = offset + 10
        dot = offset - 2

-- Helper function character parsing
offset :: Int
offset = ord '0'

-------------------------------------------------------------------------
-- * Assignment C

-- Generates an arbitrary cell
cell :: Gen (Maybe Int)
cell = frequency [(1, genNumber), (9, genNothing)]

-- Generates an empty cell
genNothing :: Gen (Maybe Int)
genNothing = return Nothing

-- Generates a random number between 1 and 9
genNumber :: Gen (Maybe Int)
genNumber = elements [Just n|n<-[1..9]]

-- An instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Property that checks if a Sudoku has valid structure and values
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------
-- * Assignment D

-- Represents a row, column or 3x3 block
type Block = [Maybe Int]

-- Checks that a Sudoku does not have duplicate integers
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock $ blocks s

-- Checks that a block does not contain duplicate integers
isOkayBlock :: Block -> Bool
isOkayBlock b = length fb == length (nub fb)
  where fb = filter isJust b

-- Formats a Sudoku into a list of all existing blocks
blocks :: Sudoku -> [Block]
blocks s = concat [rows s, cols, squares]
  where cols    = transpose $ rows s
        squares = parseSquares s

-- Parses and returns all 3x3 blocks in a Sudoku
parseSquares :: Sudoku -> [Block]
parseSquares s = [concatMap snd $
                  filter (\x -> fst x == i) zipped | i <- [0..8]]
  where zipped = zip indices splitSudoku
        indices = [ i `mod` 3 + 3 * (i `div` 9) | i <- [0..26]]
        splitSudoku = chunksOf 3 $ concat $ rows s

-------------------------------------------------------------------------
-- * Assignment D

type Pos = (Int, Int)

newtype ValidPos = ValidPos Pos
  deriving(Show)

instance Arbitrary ValidPos where
  arbitrary = do
    r <- elements [0..8]
    c <- elements [0..8]
    return $ ValidPos (r, c)

blanks :: Sudoku -> [Pos]
blanks s = concat [ [ (i,j) | j <- elemIndices Nothing (rows s !! i) ] |
                    i <- [0..8] ]

prop_blanks :: Sudoku -> Bool
prop_blanks s = and [ isNothing (s `at` ps) | ps <- blanks s]


(!!=) :: [a] -> (Int,a) -> [a]
(!!=) as (i, a) = take i as ++ [a] ++ drop (i+1) as

prop_updateAt_length :: [a] -> (Int,a) -> Bool
prop_updateAt_length as (i, a) = length as == length (as !!= (i,a))

prop_updateAt_exist :: Eq a => [a] -> (Int,a) -> Bool
prop_updateAt_exist as (i, a) = a `elem` (as !!= (i, a))

-- !!!!!!!!!!!!!!
prop_updateAt_removed :: Eq a => NonEmptyList a -> (NonNegative Int, a) -> Property
prop_updateAt_removed (NonEmpty as) (NonNegative i, a) = i <= length as ==> prev as == prev (as !!= (i,a))
  where prev xs = take i xs ++ drop (i+1) xs


update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s p v = Sudoku $ rows s !!= (fst p, row)
  where row = rows s !! fst p !!= (snd p, v)

-- !!!!!!!!!!!!!!!!!!
prop_update :: Sudoku -> ValidPos -> Maybe Int -> Bool
prop_update s (ValidPos p) v = (update s p v `at` p) == v


candidates :: Sudoku -> Pos -> [Int]
candidates s p | isJust (s `at` p) = []
candidates s p = filter (\i -> Just i `notElem` bs) [1..9]
  where bs = concat $ getBlocksAt s p

getBlocksAt :: Sudoku -> Pos -> [Block]
getBlocksAt s (r, c) = [bs !! r ,
                      bs !! (9 + c),
                      bs !! (18 + ((r `div` 3) * 3 + (c `div` 3)))]
  where bs = blocks s

at :: Sudoku -> Pos -> Maybe Int
at s p = (rows s !! fst p) !! snd p

------------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s) || not (isOkay s) = Nothing
solve s = Just $ solve' s

solve' :: Sudoku -> Sudoku
solve' s = undefined

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
