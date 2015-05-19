module Utility (selectFromRanges) where

import Data.List (foldl')
import System.Random (StdGen, random)

-- This function represents a biased probability distribution
-- where the first value of the tuple is thing that will be selected
-- and the second value of the tuple is number of chances that that
-- option will be selected, for instance [("foo", 1), ("bar", 9)]]
-- would provide 1 chance out of 10 of being selected while bar would
-- have 9 chances out of 10 of being selected
selectFromRanges :: [(a,Int)] -> StdGen -> (Maybe a, StdGen)
selectFromRanges ranges stdGen = (findVal ranges', stdGen')
    where
        findVal ((a,_):(b,y):rst)
            | randValInRange > y = Just a
            | otherwise = findVal ((b,y):rst)
        findVal ((x,_):[]) = Just x
        findVal _ = Nothing
        randValInRange = randVal `mod` totalOfRanges 
        totalOfRanges = foldl' (\x (_,y) -> x + y) 0 ranges
        (randVal, stdGen') = random stdGen 
        ranges' = foldl' genRange [] ranges 
        genRange lst@((_,prev):_) (val,cur) = (val, cur + prev) : lst
        genRange [] (val, cur) = [(val, cur)]

