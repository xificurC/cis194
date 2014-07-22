module Golf where

import Data.List
import qualified Data.Map.Lazy as M
import Data.Char (intToDigit)
    
-- 1. skips

-- takeNth - given a list and an Int returns evey nth element (one-based)
takeNth :: [a] -> Int -> [a]
takeNth lst x = reverse $ fst $ foldl' step ([],1) lst
    where step (acc,i) val = if i == x then (val:acc,1) else (acc,i+1)

-- skips - given a list returns a list of lists 
-- that contain every [1..n]th element of the inital list
skips :: [a] -> [[a]]
skips lst = zipWith takeNth (repeat lst) [1..(length lst)]
            

-- 2. localMaxima

-- part - builds a list recursively by taking n elements
-- and moving by step
-- e.g. part 3 1 [1..4] = [[1,2,3],[2,3,4]]
part :: Int -> Int -> [a] -> [[a]]
part _ _ [] = []
part n _ lst | null $ drop (n-1) lst = []
part n step lst = take n lst : part n step (drop step lst)

-- localMaxima - given a list of integers returns all which
-- are local maxima, i.e. both the left and right neighbors
-- are strictly less than the element
localMaxima :: [Integer] -> [Integer]
localMaxima lst = foldr step [] $ part 3 1 lst
    where step :: [Integer] -> [Integer] -> [Integer]
          step [left,mid,right] acc =
              if mid > left && mid > right
              then mid : acc
              else acc


-- 3. histogram

-- count frequencies of numbers - store in a map
-- generate transposed strings for numbers
-- transpose and unlines them
frequencies :: (Ord a, Num v) => [a] -> M.Map a v
frequencies = M.fromListWith (+) . (`zip` (repeat 1))
                           
buildString :: Int -> Int -> Int -> String
buildString c m n = replicate (m-c) ' ' ++ replicate c '*' ++ ['=',intToDigit n]
                           
histogram :: [Integer] -> String
histogram lst = unlines $ transpose $ map (\x -> buildString (M.findWithDefault 0 x fMap) maxMap (fromInteger x)) [0..9]
    where fMap = frequencies lst
          maxMap = M.foldl' max 0 fMap
