module Stats(
    Stats, 
    createStats, 
    createStatsShort, 
    shortStatToLong, 
    getBaseStatsForMetaType,
    statNames, 
    getStat, 
    addToStats,
    BP(..),
    MetaType(..),
    isStatMaxed,
    getMetaTypeBpCost,
    getStatsThatAreMaxed
) where

import Prelude hiding (lookup)
import Data.List (intersperse, foldl')
import qualified Data.Map as M 
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe)
import Control.Applicative

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

type StatMin = Double
type StatNaturalMax = Double
type StatMax = Double
data StatLimit = StatLimit StatMin StatNaturalMax StatMax deriving (Show)

getStatMin :: StatLimit -> StatMin
getStatMin (StatLimit statMin _ _) = statMin

getStatNaturalMax :: StatLimit -> StatNaturalMax
getStatNaturalMax (StatLimit _ statNatMax _) = statNatMax

getStatMax :: StatLimit -> StatMax
getStatMax (StatLimit _ _ statMax) = statMax

getStatLimits :: MetaType -> M.Map String StatLimit
getStatLimits metaType = M.fromList $ zip stats $ map toStatLimit $ case metaType of
    Human -> (take 8 $ repeat d) ++ [(2,12,18), (1,7,7)]
    Ork -> [(4,9,13), d, d, (3,8,12), (1,5,7), d, (1,5,7), d, (2,12,18), de]
    Dwarf -> [(2,7,10), d, (1,5,7), (3,8,12), d, d, d, (2,7,10), (2,11,16), de]
    Elf -> [d, (2,7,10), d, d, (3,8, 12), d, d, d, (2,12,18), de]
    Troll -> [(5,10,15), (1,5,7), d, (5,10,15), (1,4,6), (1,5,7), (1,5,7), d, (2,11,16), de]
    where
        stats = ["body", "agility", "reaction", "strength", "charisma", "intuition", "logic", "willpower", "initiative", "edge"]
        d = (1,6,9)
        de = (1, 6, 6)
        toStatLimit (minValue, maxValue, maxAugValue) = StatLimit minValue maxValue maxAugValue

-- If one stat is maxed, then the maximum value of the other stats will be
-- one less than the maximum
getStatsThatAreMaxed :: Stats -> MetaType -> Stats
getStatsThatAreMaxed stats metaType 
    | not . M.null $ maxedGoonStats = M.union maxedGoonStats oneLessThanMaxed
    | otherwise = M.empty
    where
        maxedGoonStats = maxed stats
        maxed goonStats = M.filterWithKey (\k _ -> isStatMaxed k metaType goonStats) goonStats
        oneLessThanMaxed = maxed statsPlusOne
        statsPlusOne = M.map (+ 1.0) stats        

isStatMaxed :: String -> MetaType -> Stats -> Bool
isStatMaxed statName metaType stats = fromMaybe False $ isMaxed <$> currentStat <*> maybeStatMax
    where
        isMaxed curStat maxStat = curStat == maxStat
        maybeStatMax = fmap getStatNaturalMax $ M.lookup statName $ getStatLimits metaType
        currentStat = M.lookup statName stats

bpCostToIncreaseStat :: String -> Stats -> MetaType -> Maybe BP
bpCostToIncreaseStat statName stats metaType = bpCost <$> statLimit <*> currentStat
    where
        statLimit = getStatLimit $ M.lookup statName $ getStatLimits metaType
        currentStat = M.lookup statName stats
        getStatLimit (Nothing) = Nothing
        getStatLimit (Just (StatLimit _ statMax _)) = Just statMax
        bpCost maxStat curStat 
            | curStat + 1 == maxStat = BP 25
            | curStat < maxStat = BP 10
            | otherwise = BP 0

addToStats :: BP -> String -> Stats -> MetaType -> (Stats, BP)
addToStats bp@(BP bpValue) statName stats metaType = statsTuple
    where
        statsTuple = fromMaybe (stats, bp) $ getStats <$> maybeBpCost <*> maybeCurrentStat
        maybeBpCost = bpCostToIncreaseStat statName stats metaType         
        (BP bpCost) = fromMaybe (BP 0) maybeBpCost
        maybeCurrentStat = M.lookup statName stats
        maybeStats = getStats <$> maybeBpCost <*> maybeCurrentStat
        getStats (BP bpCostOfStat) currentStat
            | bpCost <= bpValue = (M.insert statName (currentStat + 1) stats, BP (bpValue - bpCostOfStat))
            | otherwise = (stats, bp)

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

