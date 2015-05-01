module Stats(Stats, createStats, createStatsShort, shortStatToLong, statNames, getStat) where

import Prelude hiding (lookup)
import Data.List (intersperse, foldl')
import Data.Map (Map, fromList, lookup)
import qualified Data.Set as S
import Data.Maybe (fromJust)

type Stats = Map String Double

getStat :: String -> Stats -> Double
getStat stat stats = lookupOrError stat' stats 
    where
        stat' = lookupOrError stat shortStatToLong
        
lookupOrError :: (Ord a) => a -> Map a b -> b
lookupOrError key mp = fromJust $ lookup key mp

createStatsShort :: [(String, Double)] -> Stats
createStatsShort stats = createStats [(lookupOrError s shortStatToLong, v) | (s,v) <- stats]  

createStats :: [(String, Double)] -> Stats
createStats stats
    | all (\(s,_) -> S.member s statNames) stats = fromList stats
    | otherwise = error $ "The following values are not stats: " ++ unknownStatsString 
    where
        unknownStatsString = foldl' (++) "" $ intersperse ", " unknownStats
        unknownStats = map fst $ filter (\(s,_) -> not $ S.member s statNames) stats 

statNames :: S.Set String
statNames = S.fromList statNamesList

statNamesList :: [String]
statNamesList = ["body", "agility", "reaction", "strength", "charisma", "intuition", "logic", "willpower",
    "edge", "essense", "magic", "initiative", "initiative passes", "resonance", "condition monitor", 
    "astral initiative", "astral initiative passes", "matrix initiative"]

shortStatToLong :: Map String String
shortStatToLong = fromList $ zip ["b", "a", "r", "s", "c", "i", "l", "w", "e", "ess", "m", "init", "ip", "res", "cm", "astral init", "astral ip", "matrix init"] statNamesList

