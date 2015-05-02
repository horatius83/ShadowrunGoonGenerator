module Skills
    (
        Skill(..), 
        SkillLevel (..),
        Specialization (..),
        createSkillLevel,
        createSkill,
) where

import Data.Map (Map, findWithDefault, fromList, keys, member)
import Data.List (intersperse, foldl')

data SkillLevel = Unaware | SkillLevel Int deriving (Show)

data Specialization = Specialization String deriving (Show)

newtype Defaultable = Defaultable Bool deriving (Show)
data Skill = Skill String SkillLevel String Defaultable (Maybe Specialization) | SkillGroup String SkillLevel [Skill] deriving (Show)

createSkillLevel :: Int -> SkillLevel
createSkillLevel level 
    | level < 0 || level > 7 = error "Cannot have a skill that is higher than 7 or lower than 0."
    | otherwise = SkillLevel level 

skillName :: Skill -> String
skillName (Skill name _ _ _ _) = name
skillName (SkillGroup name _ skills) = name ++ "(" ++ skillNamesString ++ ")" 
    where
        skillNames = intersperse ", " $ map skillName skills
        skillNamesString = foldl' (++) [] skillNames

skill :: String -> Int -> String -> Bool -> Skill
skill name level la defaultable = Skill name (SkillLevel level) la (Defaultable defaultable) Nothing

notFoundSkill :: String -> Skill
notFoundSkill x = Skill ("Skill: \"" ++ x ++ "\" not found") Unaware "" (Defaultable False) Nothing

-- get a typical skill
getSkill :: String -> Skill
getSkill name = findWithDefault (notFoundSkill name) name skillsDb

createSkill :: String -> SkillLevel -> Skill
createSkill name level
    | member name skillsDb = setSkillLevel level $ findWithDefault defaultSkill name skillsDb
    | member name skillGroupsDb = setSkillLevel level $ findWithDefault defaultSkill name skillGroupsDb
    | otherwise = defaultSkill 
    where
        defaultSkill = notFoundSkill name

setSkillLevel ::  SkillLevel -> Skill -> Skill
setSkillLevel  newSkillLevel (Skill name _ linkedAttribute defaultable specialization) = Skill name newSkillLevel linkedAttribute defaultable specialization
setSkillLevel newSkillLevel (SkillGroup name _ skills) = SkillGroup name newSkillLevel $ map (setSkillLevel newSkillLevel) skills

agilitySkills :: [Skill]
agilitySkills = [skill x 0 "a" False | x <- ["Archery", "Automatics", "Blades", "Clubs", "Escape Artist", "Exotic Melee Weapon (Specific)", "Exotic Ranged Weapon (Specific)", "Forgery", "Gunnery", "Gymnastics", "Heavy Weapons", "Infiltration", "Locksmith", "Longarms", "Palming", "Pistols", "Throwing Weapons", "Unarmed Combat"]]

bodySkills :: [Skill]
bodySkills = [skill x 0 "b" False | x <- ["Diving", "Parachuting"]]

reactionSkills :: [Skill]
reactionSkills = [skill x 0 "r" y | (x,y) <- [(z, True) | z <- ["Dodge", "Pilot Ground Craft", "Pilot Watercraft"]] ++ [(z, False) | z <- ["Pilot Aerospace", "Pilot Aircraft", "Pilot Anthroform", "Pilot Exotic Vehicle (Specific)"]]]

strengthSkills :: [Skill]
strengthSkills = [skill x 0 "s" True | x <- ["Climbing", "Running", "Swimming"]]

charismaSkills :: [Skill]
charismaSkills = [skill x 0 "c" True | x <- ["Con", "Etiquette", "Instruction", "Intimidation", "Leadership", "Negotiation"]]

intuitionSkills :: [Skill]
intuitionSkills = [skill x 0 "i" y | (x,y) <- [(z,True) | z <- ["Artisan", "Disguise", "Interests Knowledge", "Navigation", "Perception", "Shadowing", "Street Knowledge", "Tracking"]] ++ [(z,False) | z <- ["Assensing", "Language"]]]

logicSkills :: [Skill]
logicSkills = [skill x 0 "l" y | (x,y) <- [(z,True) | z <- ["Academic Knowledge", "Arcana", "Armorer", "Chemistry", "Computer", "Cybercombat", "Data Search", "Demolitions", "Enchanting", "First Aid", "Hacking", "Professional Knowledge"]] ++ [(z,False) | z <- ["Aeronautics Mechanic", "Automotive Mechanic", "Cybertechnology", "Electronic Warfare", "Industrial Mechanic", "Hardware", "Medicine", "Nautical Mechanic", "Software"]]] 

willpowerSkills :: [Skill]
willpowerSkills = [skill "Astral Combat" 0 "w" False, skill "Survival" 0 "w" True]

magicSkills :: [Skill]
magicSkills = [Skill x (SkillLevel 0) "m" (Defaultable False) Nothing | x <- ["Banishing", "Binding", "Counterspelling", "Ritual Spellcasting", "Spellcasting", "Summoning"]]

resonanceSkills :: [Skill]
resonanceSkills = [Skill x (SkillLevel 0) "res" (Defaultable False) Nothing | x <- ["Compiling", "Decompiling", "Registering"]]

skillsDb :: Map String Skill
skillsDb = fromList nameSkillTuples
    where
        allSkills = foldl' (++) [] [agilitySkills, bodySkills, reactionSkills, strengthSkills, charismaSkills, intuitionSkills, logicSkills, willpowerSkills, magicSkills, resonanceSkills]
        nameSkillTuples = [(skillName x, x) | x <- allSkills] 

-- skill groups
getSkillGroup :: String -> Skill 
getSkillGroup name = findWithDefault (notFoundSkill name) name skillGroupsDb

getAllSkillGroups :: [String]
getAllSkillGroups = keys skillGroupsDb

skillGroupsList :: [(String, [String])]
skillGroupsList = [
    ("Athletics", ["Climbing", "Gymnastics", "Running", "Swimming"]),
    ("Biotech", ["Cybertechnology", "First Aid", "Medicine"]),
    ("Close Combat", ["Blades", "Clubs", "Unarmed Combat"]),
    ("Conjuring", ["Banishing", "Binding", "Summoning"]),
    ("Cracking", ["Cybercombat", "Electronic Warfare", "Hacking"]),
    ("Electronics", ["Computer", "Data Search", "Hardware", "Software"]),
    ("Firearms", ["Automatics", "Longarms", "Pistols"]),
    ("Influence", ["Con", "Etiquette", "Leadership", "Negotiation"]),
    ("Mechanic", ["Aeronautics Mechanic", "Automotive Mechanic", "Industrial Mechanic", "Nautical Mechanic"]),
    ("Outdoors", ["Navigation", "Survival", "Tracking"]),
    ("Sorcery", ["Counterspelling", "Ritual Spellcasting", "Spellcasting"]),
    ("Stealth", ["Disguise", "Infiltration", "Palming", "Shadowing"]),
    ("Tasking", ["Compiling", "Decompiling", "Registering"])] 

skillGroupsDb :: Map String Skill
skillGroupsDb = fromList [(name, parseTuple (name, skills)) | (name, skills) <- skillGroupsList] 
    where
        parseTuple (name, skills) = SkillGroup name (SkillLevel 0) $ map getSkill skills  
