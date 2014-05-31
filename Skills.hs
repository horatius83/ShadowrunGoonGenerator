module Skills
    (Skill, 
    getSkill, 
    getSpecialSkill, 
    createSkill,
    getSkillGroup,
    getAllSkillGroups,
) where

import Data.Map
import Stats

data SkillLevel = Unaware | SkillLevel Int deriving (Show)

data Specialization = Specialization String deriving (Show)

data LinkedAttribute = LinkedAttribute { laName :: String, laValue :: (Stats -> Int)} | SpecialAttribute { saName :: String, saValue :: (Stats -> Maybe Int)}
instance Show LinkedAttribute where
    show (LinkedAttribute name _) = show name
    show (SpecialAttribute name _) = show name 

data Skill = Skill { skillName :: String, skillLevel :: SkillLevel, skillLinkedAttribute :: LinkedAttribute, skillIsDefaultable :: Bool, skillSpecialization :: Maybe Specialization} deriving (Show)

skill name level la defaultable = Skill name (SkillLevel level) (LinkedAttribute attribName attribValue) defaultable Nothing
                                    where  (attribName, attribValue) = getStat la

notFoundSkill :: String -> Skill
notFoundSkill x = Skill ("Skill: \"" ++ x ++ "\" not found") Unaware (LinkedAttribute w f) False Nothing
                    where (w,f) = getStat "w"
-- get a typical skill
getSkill name = findWithDefault (notFoundSkill name) name skillDb

createSkill :: String -> Int -> Skill
createSkill name level | level <0 || level > 7 = error "Only levels between 0-7 are permitted for skills"
                       | otherwise = Skill name (SkillLevel level) la defaultable specialization
                            where (Skill _ _ la defaultable specialization) = getSkill name

agilitySkills = [skill x 0 "a" False | x <- ["Archery", "Automatics", "Blades", "Clubs", "Escape Artist", "Exotic Melee Weapon (Specific)", "Exotic Ranged Weapon (Specific)", "Forgery", "Gunnery", "Gymnastics", "Heavy Weapons", "Infiltration", "Locksmith", "Longarms", "Palming", "Pistols", "Throwing Weapons", "Unarmed Combat"]]
bodySkills = [skill x 0 "b" False | x <- ["Diving", "Parachuting"]]
reactionSkills = [skill x 0 "r" y | (x,y) <- [(z, True) | z <- ["Dodge", "Pilot Ground Craft", "Pilot Watercraft"]] ++ [(z, False) | z <- ["Pilot Aerospace", "Pilot Aircraft", "Pilot Anthroform", "Pilot Exotic Vehicle (Specific)"]]]
strengthSkills = [skill x 0 "s" True | x <- ["Climbing", "Running", "Swimming"]]
charismaSkills = [skill x 0 "c" True | x <- ["Con", "Etiquette", "Instruction", "Intimidation", "Leadership", "Negotiation"]]
intuitionSkills = [skill x 0 "i" y | (x,y) <- [(z,True) | z <- ["Artisan", "Disguise", "Interests Knowledge", "Navigation", "Perception", "Shadowing", "Street Knowledge", "Tracking"]] ++ [(z,False) | z <- ["Assensing", "Language"]]]
logicSkills = [skill x 0 "l" y | (x,y) <- [(z,True) | z <- ["Academic Knowledge", "Arcana", "Armorer", "Chemistry", "Computer", "Cybercombat", "Data Search", "Demolitions", "Enchanting", "First Aid", "Hacking", "Professional Knowledge"]] ++ [(z,False) | z <- ["Aeronautics Mechanic", "Automotive Mechanic", "Cybertechnology", "Electronic Warfare", "Industrial Mechanic", "Hardware", "Medicine", "Nautical Mechanic", "Software"]]] 
willpowerSkills = [skill "Astral Combat" 0 "w" False, skill "Survival" 0 "w" True]

skillDb = fromList [(skillName x, x) | x <- agilitySkills ++ bodySkills ++ reactionSkills ++ strengthSkills ++ charismaSkills ++ intuitionSkills ++ logicSkills ++ willpowerSkills]

-- get a special skill
getSpecialSkill name = findWithDefault (notFoundSkill name) name specialSkillDb
specialSkill :: String -> Int -> String -> Bool -> Skill
specialSkill name level la defaultable | level < 0 || level > 7 = error "Only levels between 0-7 are permitted for special skills"
                                       | otherwise = Skill name (SkillLevel level) (SpecialAttribute attribName attribValue) defaultable Nothing
                                            where (attribName, attribValue) = getSpecialStat la
magicSkills = [specialSkill x 0 "m" False | x <- ["Banishing", "Binding", "Counterspelling", "Ritual Spellcasting", "Spellcasting", "Summoning"]]
resonanceSkills = [specialSkill x 0 "res" False | x <- ["Compiling", "Decompiling", "Registering"]]
specialSkillDb = fromList [(skillName x, x) | x <- magicSkills ++ resonanceSkills] 

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

