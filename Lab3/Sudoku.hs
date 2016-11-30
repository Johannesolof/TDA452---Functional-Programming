module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function


-------------------------------------------------------------------------
-- * Assignment A

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Eq )

instance Show Sudoku where
  show = formatSudoku

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
-- * Assignment E

type Pos = (Int, Int)

newtype ValidPos = ValidPos Pos
  deriving(Show)

instance Arbitrary ValidPos where
  arbitrary = do
    r <- elements [0..8]
    c <- elements [0..8]
    return $ ValidPos (r, c)

{-
  We define our updateAt-properties for the Sudoku case where the list is
  never empty and the index is always valid and thus we want our test data
  to fulfill this as well. Also, in our intact property, to make meaningful
  tests we need the values to differentiate from each other which is why we
  pick Int as the type. This would otherwise default to unit.
-}
data UpdateAtData = UAData [Int] (Int, Int)
  deriving(Show)

-- Generates a non empty list, a valid index for that list and an arbitrary int
instance Arbitrary UpdateAtData where
    arbitrary = do
      ns  <- listOf1 arbitrary
      i   <- elements [0..length ns - 1]
      n   <- arbitrary
      return $ UAData ns (i, n)

-- Returns the postions of all blank cells for the given Sudoku
blanks :: Sudoku -> [Pos]
blanks s = concat [ [ (i,j) | j <- elemIndices Nothing (rows s !! i) ] |
                    i <- [0..8] ]

-- Check that the postions returned by blanks are actually blank
prop_blanks :: Sudoku -> Bool
prop_blanks s = and [isNothing (s `at` ps) | ps <- blanks s]

-- Updates the value at the given index and returns the resulting list
-- This operator is referred to as 'updateAt' in the following properties
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) as (i, a) = take i as ++ [a] ++ drop (i+1) as

-- Check that lists before and after updateAt have the same length
prop_updateAt_length :: UpdateAtData -> Bool
prop_updateAt_length (UAData as (i,a)) = length as == length (as !!= (i,a))

-- Check that the value at i has actually been updated
prop_updateAt_exist :: UpdateAtData -> Bool
prop_updateAt_exist (UAData as (i,a)) = (as !!= (i, a)) !! i == a

-- Check that all values (except from the updated) that existed in the
-- list before the update still exists afterwards
prop_updateAt_intact :: UpdateAtData -> Bool
prop_updateAt_intact (UAData as (i,a)) = prev as == prev (as !!= (i, a))
  where prev xs = take i xs ++ drop (i+1) xs

-- Update position in sudoku with new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s p v = Sudoku $ rows s !!= (fst p, row)
  where row = rows s !! fst p !!= (snd p, v)

-- Check that the value is actually updated after update
prop_update :: Sudoku -> ValidPos -> Maybe Int -> Bool
prop_update s (ValidPos p) v = (update s p v `at` p) == v

-- Returns the possible values that could be assinged to pos without
-- making the Sudoku invalid
candidates :: Sudoku -> Pos -> [Int]
candidates s p | isJust (s `at` p) = []
candidates s p = filter (\i -> Just i `notElem` bs) [1..9]
  where bs = concat $ getBlocksAt s p

-- Check that none of the candidates makes the Sudoku invalid
prop_candidates :: Sudoku -> ValidPos -> Property
prop_candidates s (ValidPos p) = isNothing (s `at` p) && isOkay s ==>
  and $ map valid $ candidates s p
    where valid c = isOkay $ update s p (Just c)

-- Returns the row, column and 3x3 block connected to the given pos
getBlocksAt :: Sudoku -> Pos -> [Block]
getBlocksAt s (r, c) = [bs !! r ,
                      bs !! (9 + c),
                      bs !! (18 + ((r `div` 3) * 3 + (c `div` 3)))]
  where bs = blocks s

-- Returns the value at pos from the given Sudoku
at :: Sudoku -> Pos -> Maybe Int
at s p = (rows s !! fst p) !! snd p

------------------------------------------------------------------------------
-- * Assignment F

-- Helper function that checks both structure and values for a Sudoku
isValidSud :: Sudoku -> Bool
isValidSud s = isSudoku s && isOkay s

-- Attempts to solve a sudoku
solve :: Sudoku -> Maybe Sudoku
solve s | isValidSud s = solve' s
        | otherwise    = Nothing

-- Helper function for solve that does the actual solving
-- Optimization: builds the solution by picking the blank with
-- the least candidates in each step
solve' :: Sudoku -> Maybe Sudoku
solve' s | null (blanks s)   = Just s
         | otherwise = solveCandidates s blank
  where blank = minimumBy (compare `on` (length . snd))
                [(b, candidates s b) | b <- blanks s]

-- Builds a solution from the given pos and its candidates as starting point
solveCandidates :: Sudoku -> (Pos, [Int]) -> Maybe Sudoku
solveCandidates _ (_, []) = Nothing
solveCandidates s (p, cs) = listToMaybe $ catMaybes
                            [solve' $ update s p (Just c) | c <- cs]

-- Reads Sudoku from file and tries to solve it
readAndSolve :: FilePath -> IO ()
readAndSolve fp =
  do
    sud <- readSudoku fp
    printSudoku sud
    let solvedSud = solve sud
    maybe (putStrLn "Failed to solve") printSudoku solvedSud
    return ()

-- Helper function, checks whether there are still blanks in a Sudoku
noBlanks :: Sudoku -> Bool
noBlanks = null . blanks

-- Checks whether the first Sudoku is a valid solution to the second one
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol org | isValidSud sol && noBlanks sol = compareSols
                     | otherwise                      = False
   where compareSols = sol == fillBlanks sol org

-- Fills the blanks in the second sud with corresponding values in the first one
fillBlanks :: Sudoku -> Sudoku -> Sudoku
fillBlanks sol org | noBlanks org = org
                   | otherwise = fillBlanks (update org pos val) sol
   where pos = head $ blanks org
         val = sol `at` pos

-- Check that solve only produces valid solutions
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isValidSud s ==> fromJust (solve s) `isSolutionOf` s


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
