module Goon (getGoon, selectFromRanges, selectMetaType) where

import Cyberware (Cyberware(..), BodyPart(..), CyberLimbEnhancement(..))
import Hacking (Program)
import Stats (Stats, createStatsShort, BP(..), MetaType(..), getStatsThatAreMaxed, addToStats)
import Weapons (Weapon, getWeapon)
import Skills (Skill(..), createSkill, SkillLevel(..))
import Spells (Spell, getSpell)
import Armor (getArmor)
import Equipment (Equipment(..), getEquipment, FocusType(..))
import Qualities (qualityDb)
import System.Random (mkStdGen)
import Prelude hiding (init, foldl)
import Data.Maybe (fromJust, isJust)
import Utility (selectFromRanges)
import Data.Hashable (hash)
import Qualities (Quality(..), qualityDb)
import qualified Data.Map as M

data Goon = Goon {
    name :: String,
    rating :: Int,
    stats :: Stats,
    skills :: [Skill],
    weapons :: [Weapon],
    equipment :: [Equipment],
    cyberware :: Maybe [Cyberware],
    programs :: Maybe [Program],
    spells :: Maybe [Spell],
    metatype :: MetaType} deriving (Show)

data GoonType = Hacker | Magician | Berzerker | Gunman | Face | Pilot deriving (Show)

generateGoon :: GoonType -> BP -> String -> Goon
generateGoon goonType bp goonName = undefined 
    where
        -- generate metatype
        seed = fromIntegral $ hash goonName
        metaType = selectMetaType goonType seed
        -- generate qualities
        qualities = getQualitiesByMetaType goonType
        -- assign BPs to attributes
        -- assign BPs to skills
        -- assign BPs to resources, gear etc.
        
selectMetaType :: GoonType -> Int -> MetaType
selectMetaType goonType seed = fromJust . fst $ selectFromRanges ranges (mkStdGen seed)  
    where
        ranges = getRange $ case goonType of
            Hacker -> [30, 10, 25, 25, 10]
            Magician -> [20, 10, 20, 40, 10]
            Berzerker -> [10, 20, 10, 10, 50]
            Gunman -> [20, 20, 20, 20, 20]
            Face -> [35, 10, 10, 35, 10]
            Pilot -> [20, 15, 30, 20, 15]
        getRange probabilities = zip [Human .. Troll] probabilities 

{--getQualitiesByMetaType :: GoonType -> [Quality]
getQualitiesByMetaType goonType = getQuality qualityNames
    where
        getQuality [] = []
        getQuality names = fmap fromJust $ filter isJust $ fmap (\x -> M.lookup x qualityDb) names
        qualityNames  = case goonType of
            Hacker -> []
            Magician -> ["Magician"]
            Berzerker -> ["High Tolerance 3"]
            Gunman ->[]
            Face -> ["First Impression", "Blandness"]
            Pilot -> []
  --}  
getQualitiesByMetaType :: GoonType -> BP -> [Quality]
getQualitiesByMetaType goonType bp = undefined
    where
        qualityNames = case goonType of
            Hacker -> [[CodeSlinger "Matrix Attack"], [ExceptionalAttribute "Logic"]]
            Magician -> (map (map toQuality) [["Magician"], ["Focused Concentration 1", "Focused Concentration 2"]]) ++ [[ExceptionalAttribute "Magic"]]
            Berzerker -> (map (map toQuality) [["High Pain Tolerance" ++ (show i) | i <- [1..3]]]) ++ [[ExceptionalAttribute "Strength"]]
            Gunman -> [[ExceptionalAttribute "Agility"]]
        toQuality x = fromJust $ M.lookup x qualityDb 

addStats :: BP -> GoonType -> MetaType -> Stats -> Int -> Maybe (Stats, BP)
addStats bp goonType metaType goonStats seed = getStatsAndBpFromStatName maybeStatName
    where
        getStatsAndBpFromStatName (Just goonName) = addToStats bp goonName goonStats metaType
        getStatsAndBpFromStatName (Nothing) = Nothing
        (maybeStatName, _) = selectFromRanges filteredRanges $ mkStdGen seed
        -- Get the stats that are maxed out, and filter those out
        filteredRanges = filter (\(stat, _) -> not $ stat `M.member` statsThatAreMaxed) ranges 
        -- If one stat is maxed then we cannot max out any other stats, so remove any that are one below max as well
        statsThatAreMaxed = getStatsThatAreMaxed goonStats metaType
        ranges = zip statNames $ case goonType of
                        -- b   a   r   s   c   i   l   w  m  r
            Hacker ->    [10, 20, 10, 10, 10, 10, 40, 10, 0, 30]
            Magician ->  [10, 10, 10, 10, 10, 10, 20, 10, 40, 0]
            Berzerker -> [20, 10, 10, 40, 10, 10, 10, 10, 0, 0]
            Gunman ->    [10, 40, 10, 10, 10, 10, 10, 10, 0, 0]
            Face ->      [10, 10, 10, 10, 40, 10, 10, 10, 0, 0]
            Pilot ->     [10, 10, 40, 10, 10, 10, 10, 10, 0, 0]
        statNames = ["body", "agility", "reaction", "strength", "charisma", "intuition", "logic", "willpower", "magic", "resonance"]

createGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createGoonStats b a r s c i l w init = createStatsShort $ createBaseStats b a r s c i l w 1 init 1 10 

createMagicGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createMagicGoonStats b a r s c i l w ess m init ip cm astralInit astralIp = createStatsShort $ statTuples ++ magicalStatTuples 
    where
        magicalStatTuples = zip ["m", "astral init", "astral ip"] $ map fromIntegral [m, astralInit, astralIp] 
        statTuples = createBaseStats b a r s c i l w ess init ip cm 

createGoon :: String -> Int -> Stats -> [(String, Int)] -> [String] -> [String] -> [String] -> Maybe [Cyberware] -> Maybe [Program] -> Maybe [Spell] -> Goon
createGoon goonName rank goonStats goonSkills goonWeapons armor goonEquipment goonCyberware goonPrograms goonSpells = Goon goonName rank goonStats skills' weapons' equipment' goonCyberware goonPrograms goonSpells Human
    where
        skills' = [createSkill x (SkillLevel y) | (x,y) <- goonSkills]
        weapons' = [getWeapon x | x <- goonWeapons]
        equipment' = [getArmor x | x <- armor] ++ [getEquipment x | x <- goonEquipment]

createSkillList :: [(String, Int)] -> [Skill]
createSkillList goonSkills = [createSkill x (SkillLevel y) | (x,y) <- goonSkills]

createBaseStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> [(String,Double)]
createBaseStats b a r s c i l w ess init ip cm = statsTuples ++ [("ess", ess)] 
    where
        statsTuples = zip ["b", "a", "r", "s", "c", "i", "l", "w", "e", "init", "ip", "cm"] statsList 
        statsList = map fromIntegral [b,a,r,s,c,i,l,w,1,init,ip,cm] 

getGoon :: String -> Goon
getGoon goonName = undefined

rentacop :: Goon
rentacop = createGoon "Corporate Security Unit" 2 goonStats goonSkills goonWeapons armor goonEquipment Nothing Nothing Nothing 
    where
        goonStats = createGoonStats 3 3 4 3 3 3 2 3 7 
        goonSkills = [("Athletics", 3), ("Automatics", 3), ("Dodge", 3), ("Pistols", 3), ("Close Combat", 3)]
        goonWeapons = ["Fischetti Security 600 Light Pistol", "HK-227X", "Stun Baton"]
        armor = ["Armor Vest"]
        goonEquipment = ["CMT Clip"]

rentaCopLt :: Goon
rentaCopLt = Goon "CorpSec Lieutenant" 2 goonStats goonSkills goonWeapons goonEquipment (Just goonCyberware) Nothing (Just goonSpells) Human
    where
        goonStats = createMagicGoonStats 3 3 3 3 3 4 3 4 6.0 3 7 1 10 8 3
        goonSkills = createSkillList [("Assensing", 3), ("Astral Combat", 4), ("Conjuring", 3), ("Leadership", 2), ("Pistols", 2), ("Sorcery", 4)]
        goonWeapons = [getWeapon "Fischetti Security 600 Light Pistol"]
        goonEquipment = [getArmor "Armor Vest", getEquipment "Renraku Sensei", Focus Spellcasting 2] 
        goonCyberware = [CyberLimb "Full Arm" Arm (Just $ Body 1) 1.0 15 []]
        goonSpells = map getSpell ["Detect Life", "Light", "Physical Barrier", "Powerbolt", "Silence", "Stunball"]        
