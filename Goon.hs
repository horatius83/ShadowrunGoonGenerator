module Goon (getGoon) where

import Cyberware (Cyberware(..), BodyPart(..), CyberLimbEnhancement(..))
import Hacking (Program)
import Stats (Stats, createStatsShort)
import Weapons (Weapon, getWeapon)
import Skills (Skill(..), createSkill, SkillLevel(..))
import Spells (Spell, getSpell)
import Armor (getArmor)
import Equipment (Equipment(..), getEquipment, FocusType(..))

import Prelude hiding (init, foldl)
import Data.Map (empty, insertWith, insert, fromList, member, (!), foldlWithKey)
import Data.List (foldl')

data Goon = Goon {
    goonName :: String,
    goonRating :: Int,
    goonStats :: Stats,
    goonSkills :: [Skill],
    goonWeapons :: [Weapon],
    goonEquipment :: [Equipment],
    goonCyberware :: Maybe [Cyberware],
    goonPrograms :: Maybe [Program],
    goonSpells :: Maybe [Spell]} deriving (Show)

data GoonType = Hacker | Magician | Berzerker | Gunman | Face | Pilot deriving (Show)

generateGoon :: String -> [Skill] -> Goon
generateGoon name skills = undefined
-- If he has skills in magic, then he's a magic goon
--  determine what stats to raise for magic
--  raise those stats based on the rating
--  choose random spells that correspond to those skills
-- If he has skills in hacking, then he's a hacker
--  determine what stats to raise for a hacker
--  choose random programs to correspond to those skills
-- Otherwise he's a physical goon
-- A physical goon can be a melee goon, ranged goon, or heavy goon
-- melee: strong melee weapon, with a pistol or heavy pistol for backup, medium-heavy armor (depending on rating)
-- ranged: strong ranged weapon, a melee weapon for backup, light-heavy armor (depending on rating)
-- heavy: strong ranged weapon (Grenade launcher / Rockets at the higher level), strong melee weapon, heavy armor

getGoonType :: [Skill] -> GoonType
getGoonType skills = linkedAttributes ! (fst maxAttribute) 
    where
        maxAttribute = foldlWithKey (\a@(attrib, level) attrib' level' -> if level' > level then (attrib', level') else a) ("a", 0) skillsCount 
        linkedAttributes =  fromList [("b", Berzerker), ("a", Gunman), ("r", Pilot), ("s", Berzerker), ("c", Face), ("i", Hacker), ("w", Magician), ("l", Hacker), ("m", Magician), ("res", Hacker)]
        skillsCount = foldl' countGoonType empty $ filter isLinkedAttribute skills
        isLinkedAttribute (Skill _ _ stat _ _) = stat `member` linkedAttributes
        isLinkedAttribute (SkillGroup _ _ skillGroupSkills) = all isLinkedAttribute skillGroupSkills
        countGoonType skillMap (Skill _ (SkillLevel level) linkedStat _ _)
            | linkedStat `member` skillMap = insertWith (+) linkedStat level skillMap
            | otherwise = insert linkedStat level skillMap 
        countGoonType skillMap (SkillGroup _ _ groupSkills) = foldl' countGoonType skillMap groupSkills 
        countGoonType skillMap _ = skillMap 

createGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createGoonStats b a r s c i l w init = createStatsShort $ createBaseStats b a r s c i l w 1 init 1 10 

createMagicGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createMagicGoonStats b a r s c i l w ess m init ip cm astralInit astralIp = createStatsShort $ statTuples ++ magicalStatTuples 
    where
        magicalStatTuples = zip ["m", "astral init", "astral ip"] $ map fromIntegral [m, astralInit, astralIp] 
        statTuples = createBaseStats b a r s c i l w ess init ip cm 

createGoon :: String -> Int -> Stats -> [(String, Int)] -> [String] -> [String] -> [String] -> Maybe [Cyberware] -> Maybe [Program] -> Maybe [Spell] -> Goon
createGoon name rank stats skills weapons armor equipment cyberware programs spells = Goon name rank stats skills' weapons' equipment' cyberware programs spells
    where
        skills' = [createSkill x (SkillLevel y) | (x,y) <- skills]
        weapons' = [getWeapon x | x <- weapons]
        equipment' = [getArmor x | x <- armor] ++ [getEquipment x | x <- equipment]

createSkillList :: [(String, Int)] -> [Skill]
createSkillList skills = [createSkill x (SkillLevel y) | (x,y) <- skills]

createBaseStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> [(String,Double)]
createBaseStats b a r s c i l w ess init ip cm = statsTuples ++ [("ess", ess)] 
    where
        statsTuples = zip ["b", "a", "r", "s", "c", "i", "l", "w", "e", "init", "ip", "cm"] statsList 
        statsList = map fromIntegral [b,a,r,s,c,i,l,w,1,init,ip,cm] 

getGoon :: String -> Goon
getGoon name = undefined

rentacop :: Goon
rentacop = createGoon "Corporate Security Unit" 2 stats skills weapons armor equipment Nothing Nothing Nothing 
    where
        stats = createGoonStats 3 3 4 3 3 3 2 3 7 
        skills = [("Athletics", 3), ("Automatics", 3), ("Dodge", 3), ("Pistols", 3), ("Close Combat", 3)]
        weapons = ["Fischetti Security 600 Light Pistol", "HK-227X", "Stun Baton"]
        armor = ["Armor Vest"]
        equipment = ["CMT Clip"]

rentaCopLt :: Goon
rentaCopLt = Goon "CorpSec Lieutenant" 2 stats skills weapons equipment (Just cyberware) Nothing (Just spells)
    where
        stats = createMagicGoonStats 3 3 3 3 3 4 3 4 6.0 3 7 1 10 8 3
        skills = createSkillList [("Assensing", 3), ("Astral Combat", 4), ("Conjuring", 3), ("Leadership", 2), ("Pistols", 2), ("Sorcery", 4)]
        weapons = [getWeapon "Fischetti Security 600 Light Pistol"]
        equipment = [getArmor "Armor Vest", getEquipment "Renraku Sensei", Focus Spellcasting 2] 
        cyberware = [CyberLimb "Full Arm" Arm (Just $ Body 1) 1.0 15 []]
        spells = map getSpell ["Detect Life", "Light", "Physical Barrier", "Powerbolt", "Silence", "Stunball"]        

printGoon :: Goon -> IO ()
printGoon goon = do
    putStrLn $ "Name: " ++ (goonName goon)
    putStrLn "Stats:"
     
