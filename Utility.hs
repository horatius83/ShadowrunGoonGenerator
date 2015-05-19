module Utility (selectFromRanges) where

import Data.List (foldl')
import System.Random (StdGen, random)

selectFromRanges :: [(a,Int)] -> StdGen -> (Maybe a, StdGen)
selectFromRanges ranges stdGen = (findVal ranges', stdGen')
    where
        findVal ((a,x):(b,y):rst)
            | randValInRange > y = Just a
            | otherwise = findVal ((b,y):rst)
        findVal ((x,_):[]) = Just x
        findVal _ = Nothing
        randValInRange = randVal `mod` totalOfRanges 
        totalOfRanges = foldl' (\x (_,y) -> x + y) 0 ranges
        (randVal, stdGen') = random stdGen 
        ranges' = foldl' genRange [] ranges 
        genRange lst@((_,prev):rst) (val,cur) = (val, cur + prev) : lst
        genRange [] (val, cur) = [(val, cur)]

