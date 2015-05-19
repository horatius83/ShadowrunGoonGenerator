module Stats(
    Stats, 
    createStats, 
    createStatsShort, 
    shortStatToLong, 
    statNames, 
    getStat, 
    BP(..),
    MetaType(..),
    getMetaTypeBpCost
) where

import Prelude hiding (lookup)
import Data.List (intersperse, foldl')
import qualified Data.Map as M 
import qualified Data.Set as S
import Data.Maybe (fromJust)

type Stats = M.Map String Double
newtype BP = BP Int deriving (Show)

data MetaType = Human | Ork | Dwarf | Elf | Troll deriving (Show, Enum)

getMetaTypeBpCost :: MetaType -> BP 
getMetaTypeBpCost metaType = BP $ case metaType of
    Human -> 0
    Ork -> 20
    Dwarf -> 25
    Elf -> 30
    Troll -> 40


data StatLimit = StatLimit Double Double Double deriving (Show)

getStatLimits :: MetaType -> M.Map String StatLimit
getStatLimits metaType = M.fromList $ zip stats $ map toStatLimit $ case metaType of
    Human -> (take 8 $ repeat d) ++ [(2,12,18)]
    Ork -> [(4,9,13), d, d, (3,8,12), (1,5,7), d, (1,5,7), d, (2,12,18)]
    Dwarf -> [(2,7,10), d, (1,5,7), (3,8,12), d, d, d, (2,7,10), (2,11,16)]
    Elf -> [d, (2,7,10), d, d, (3,8, 12), d, d, d, (2,12,18)]
    Troll -> [(5,10,15), (1,5,7), d, (5,10,15), (1,4,6), (1,5,7), (1,5,7), d, (2,11,16)]
    where
        stats = ["body", "agility", "reaction", "strength", "charisma", "intuition", "logic", "willpower", "initiative"]
        d = (1,6,9)
        toStatLimit (minValue, maxValue, maxAugValue) = StatLimit minValue maxValue maxAugValue

getBaseStatsForMetaType :: MetaType -> Stats
getBaseStatsForMetaType metaType = M.map getMin $ statLimits
    where
        getMin (StatLimit minValue _ _) = minValue
        statLimits = getStatLimits metaType


getStat :: String -> Stats -> Double
getStat stat stats = lookupOrError stat' stats 
    where
        stat' = lookupOrError stat shortStatToLong
        
lookupOrError :: (Ord a) => a -> M.Map a b -> b
lookupOrError key mp = fromJust $ M.lookup key mp

createStatsShort :: [(String, Double)] -> Stats
createStatsShort stats = createStats [(lookupOrError s shortStatToLong, v) | (s,v) <- stats]  

createStats :: [(String, Double)] -> Stats
createStats stats
    | all (\(s,_) -> S.member s statNames) stats = M.fromList stats
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

shortStatToLong :: M.Map String String
shortStatToLong = M.fromList $ zip ["b", "a", "r", "s", "c", "i", "l", "w", "e", "ess", "m", "init", "ip", "res", "cm", "astral init", "astral ip", "matrix init"] statNamesList

