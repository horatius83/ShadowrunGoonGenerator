module Skills
    (Skill, 
    getSkill, 
    createSkill,
    getSkillGroup,
    getAllSkillGroups,
) where

import Data.Map (Map, findWithDefault, fromList, keys)
import Data.List (intersperse, foldl')
import Stats

data SkillLevel = Unaware | SkillLevel Int deriving (Show)

data Specialization = Specialization String deriving (Show)

data Skill = Skill String SkillLevel String Bool (Maybe Specialization) | SkillGroup String Int [Skill] deriving (Show)

skillName :: Skill -> String
skillName (Skill name _ _ _ _) = name
skillName (SkillGroup name _ skills) = name ++ "(" ++ skillNamesString ++ ")" 
    where
        skillNames = intersperse ", " $ map skillName skills
        skillNamesString = foldl' (++) [] skillNames

skill :: String -> Int -> String -> Bool -> Skill
skill name level la defaultable = Skill name (SkillLevel level) la defaultable Nothing

notFoundSkill :: String -> Skill
notFoundSkill x = Skill ("Skill: \"" ++ x ++ "\" not found") Unaware "" False Nothing

-- get a typical skill
getSkill name = findWithDefault (notFoundSkill name) name skillDb

createSkill :: String -> SkillLevel -> Skill
createSkill name level = undefined

agilitySkills = [skill x 0 "a" False | x <- ["Archery", "Automatics", "Blades", "Clubs", "Escape Artist", "Exotic Melee Weapon (Specific)", "Exotic Ranged Weapon (Specific)", "Forgery", "Gunnery", "Gymnastics", "Heavy Weapons", "Infiltration", "Locksmith", "Longarms", "Palming", "Pistols", "Throwing Weapons", "Unarmed Combat"]]

bodySkills = [skill x 0 "b" False | x <- ["Diving", "Parachuting"]]

reactionSkills = [skill x 0 "r" y | (x,y) <- [(z, True) | z <- ["Dodge", "Pilot Ground Craft", "Pilot Watercraft"]] ++ [(z, False) | z <- ["Pilot Aerospace", "Pilot Aircraft", "Pilot Anthroform", "Pilot Exotic Vehicle (Specific)"]]]

strengthSkills = [skill x 0 "s" True | x <- ["Climbing", "Running", "Swimming"]]

charismaSkills = [skill x 0 "c" True | x <- ["Con", "Etiquette", "Instruction", "Intimidation", "Leadership", "Negotiation"]]

intuitionSkills = [skill x 0 "i" y | (x,y) <- [(z,True) | z <- ["Artisan", "Disguise", "Interests Knowledge", "Navigation", "Perception", "Shadowing", "Street Knowledge", "Tracking"]] ++ [(z,False) | z <- ["Assensing", "Language"]]]

logicSkills = [skill x 0 "l" y | (x,y) <- [(z,True) | z <- ["Academic Knowledge", "Arcana", "Armorer", "Chemistry", "Computer", "Cybercombat", "Data Search", "Demolitions", "Enchanting", "First Aid", "Hacking", "Professional Knowledge"]] ++ [(z,False) | z <- ["Aeronautics Mechanic", "Automotive Mechanic", "Cybertechnology", "Electronic Warfare", "Industrial Mechanic", "Hardware", "Medicine", "Nautical Mechanic", "Software"]]] 

willpowerSkills = [skill "Astral Combat" 0 "w" False, skill "Survival" 0 "w" True]

magicSkills = [Skill x (SkillLevel 0) "m" False Nothing | x <- ["Banishing", "Binding", "Counterspelling", "Ritual Spellcasting", "Spellcasting", "Summoning"]]

resonanceSkills = [Skill x (SkillLevel 0) "res" False Nothing | x <- ["Compiling", "Decompiling", "Registering"]]

skillDb :: Map String Skill
skillDb = fromList nameSkillTuples
    where
        allSkills = foldl' (++) [] [agilitySkills, bodySkills, reactionSkills, strengthSkills, charismaSkills, intuitionSkills, logicSkills, willpowerSkills, magicSkills, resonanceSkills]
        nameSkillTuples = [(skillName x, x) | x <- allSkills] 

-- skill groups
getSkillGroup :: String -> [String]
getSkillGroup name = findWithDefault [] name skillGroupsDb
getAllSkillGroups = keys skillGroupsDb

skillGroupsDb = fromList [
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

