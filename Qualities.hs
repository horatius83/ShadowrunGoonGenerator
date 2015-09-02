module Qualities (qualityDb, Quality(..), getQualityCost) where

import Data.Map (Map, fromList, lookup)
import Stats (BP(..), addBP, MetaType(..))
import Control.Applicative ((<$>), (<*>))
import Prelude hiding (lookup)

type Name = String
type Description = String
type Toxin = String
data AddictionLevel = Mild | Moderate | Severe | Burnout deriving (Show)
data Level = One | Two deriving (Show)

data Quality = Quality Name BP Description (Maybe [String]) (Maybe [String])
    | Aptitude String
    | CodeSlinger String
    | ExceptionalAttribute String 
    | Addiction String AddictionLevel
    | Immunity Toxin Level
    | CoderBlock String
    | Incompetent String deriving (Show)

qualityName :: Quality -> String
qualityName q = case q of
    (Aptitude skill) -> "Aptitude for " ++ show skill
    (CodeSlinger matrixAction) -> "CodeSlinger at " ++ show matrixAction
    (ExceptionalAttribute attribute) -> "Exceptional " ++ show attribute
    (Addiction substance level) -> "Addicted to " ++ substance ++ "(" ++ (show level) ++ ")"
    (Immunity toxin level) -> "Natural immunity to " ++ (show toxin) ++ "(" ++ (show level) ++ ")"
    (CoderBlock matrixAction) -> "CoderBlock at " ++ show matrixAction
    (Incompetent skill) -> "Incompetent at " ++ show skill
    (Quality name _ _ _ _) -> name

qualityBp :: Quality -> BP
qualityBp q = case q of
    (Aptitude _) -> BP 10
    (CodeSlinger _) -> BP 10
    (ExceptionalAttribute _) -> BP 20
    (Addiction _ Mild) -> BP (-5)
    (Addiction _ Moderate) -> BP (-10)
    (Addiction _ Severe) -> BP (-20)
    (Addiction _ Burnout) -> BP (-30)
    (Immunity _ One) -> BP 5
    (Immunity _ Two) -> BP 10
    (CoderBlock _) -> BP (-5)
    (Incompetent _) -> BP (-5) 
    (Quality _ bp _ _ _) -> bp

getQualityCost :: Quality -> [Quality] -> BP
getQualityCost q qualities
    | isTechnomancer && (nameOfQuality == "Scorched" || nameOfQuality == "Sensitive Neural Structure") = addBP bpCost bpCost
    | isTechnomancer && nameOfQuality == "Simsense Vertigo" = BP (-15)
    | otherwise = bpCost
    where
        isTechnomancer = "Technomancer" `elem` map qualityName qualities 
        bpCost = qualityBp q
        nameOfQuality = qualityName q

getAvailableQualities :: [Quality] -> MetaType -> Map String Quality
getAvailableQualities currentQualities metaType = undefined
    
quality :: String -> Int -> String -> Quality
quality name bp description = Quality name (BP bp) description Nothing Nothing

qualityDb :: Map String Quality
qualityDb = fromList [(qualityName q, q) | q <- [
    Quality "Adept" (BP 5) "Adept characters channel magic through their bodies, Magic value starts at 1, can be increased up to 6. Power points equal Magic Value" (Just ["Magician", "Mystic Adept", "Technomancer"]) Nothing,
    quality "Ambidexterous" 5 "Character does not suffer modifiers for using an off-hand weapon",
    quality "Animal Empathy" 10 "+2 dice pool modifier for all tests involving the influence or control of an animal. Does not work on sentient creatures.",
    quality "Aptitude" 10 "Characters may improve one skill above its' natural value to 7. Increasing beyond 6 costs double the normal Karma cost or double the BP cost.",
    Quality "Astral Chameleon" (BP 5) "All astral signatures left by the character last half as long, assensing this character takes a -2 dice pool modifier" Nothing (Just ["Adept", "Magician", "Mystic Adept"]),
    quality "Blandness" 10 "Individuals attempting to shadow / find this character physically though social means or in a crowded area receive a -2 dice pool modifier",
    quality "Codeslinger" 10 "Adept at performing a particular matrix action. Receive +2 dice pool for that action",
    quality "Double Jointed" 5 "+2 dice pool modifier for Escape Artist Tests. Maybe be able squeeze into small spaces.",
    quality "Erased 1" 5 "Criminal SINs and unwanted data disappear within a week.",
    quality "Erased 2" 10 "Any SIN, undesireable information is burnt after 24 hours",
    quality "Exceptional Attribute" 20 "Increases the maximum for a particular attribute",
    quality "First Impression" 5 "+2 dice pool when infiltrating organizations, making contacts, or generally meeting people for the first time",
    Quality "Focused Concentration 1" (BP 10) "+1 dice pool modifier for all drain tests" Nothing (Just ["Magician", "Mystic Adept"]),
    Quality "Focused Concentration 2" (BP 20) "+2 dice pool modifier for all drain tests" Nothing (Just ["Magician", "Mystic Adept"]),
    quality "Guts" 5 "+2 dice pool on tests made to resist fear and intimidation (including magically induced terror)",
    Quality "High Pain Tolerance 1" (BP 5) "Ignore one box of damage when calculating wound modifiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    Quality "High Pain Tolerance 2" (BP 10) "Ignore two boxes of damage when calculating wound modifiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    Quality "High Pain Tolerance 3" (BP 15) "Ignore three boxes of damage when calculating wound modifiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    quality "Home Ground" 10 "+2 dice pool for all Active Skills where tests are made in this characters home turf",
    quality "Human-Looking" 5 "This metahuman character can 'pass' as a human. Only elves, dwarfs and orks can take this quality",
    quality "Lucky" 20 "Increases maximum edge by 1",
    Quality "Magician" (BP 20) "Can cast spells, summon spirits etc." (Just ["Adept", "Mystic Adept", "Technomancer"]) Nothing,
    quality "Magic Resitance 1" 5 "Receive one additional die for magic resistance tests (this applies to healing as well)",
    quality "Magic Resitance 2" 10 "Receive two additional dice for magic resistance tests (this applies to healing as well)",
    quality "Magic Resistance 3" 15 "Receive three additional dice for magic resistance tests (this applies to healing as well)",
    quality "Magic Resistance 4" 20 "Receive four additional dice for magic resistance tests (this applies to healing as well)",
    Quality "Mentor Spirit" (BP 5) "This character has a patron mentor spirit" Nothing (Just ["Magician", "Adept", "Mystic Adept"]),
    quality "Murky Link" 10 "Any ritual sorcery directed against this character receives a -3 dice pool modifier.",
    Quality "Mystic Adept" (BP 10) "A hybrid between mystics and adepts." (Just ["Adept", "Magician", "Technomancer"]) Nothing,
    quality "Natural Hardening" 10 "Gives character one point of biofeedback filtering.",
    quality "Natural Immunity 1" 5 "An innate or developed immunity to one single disease or toxin. Does not affect diseases or toxins that are magic based.",
    quality "Natural Immunity 2" 15 "An innate or developed immunity to one single synthetic disease or toxin. Does not affect diseases or toxins that are magic based.",
    quality "Photographic Memory" 10 "When making memory tests, the character gains -1 threshold modifier to the test.",
    quality "Quick Healer" 10 "The character receives +2 dice pool modifier to all Healing Tests made on/for/by them, including magical healing",
    quality "Resistance to Pathogens or Toxins" 5 "Receive a +1 dice pool modifier to resistance tests to pathogens (exclusive or) toxins",
    quality "Resistance to Pathogens and Toxins" 10 "Receive a +1 dice pool modifier to resistance tests to pathogens and toxins",
    Quality "Technomancer" (BP 5) "Can access or manipulate the matrix through sheer force of will. Start with Resonance attribute of 1 that can be increased to a maximum of 6" (Just ["Magician", "Adept", "Mystic Adept"]) Nothing,
    quality "Toughness" 10 "Gain +1 dice pool modifier to Body when making Damage Resistance Tests",
    quality "Will To Live 1" 5 "The character gains 1 additional damage overflow box. Does not raise the threshold at which the character becomes incompacitated, or wound modifiers from damage taken.",
    quality "Will To Live 2" 10 "The character gains 2 additional damage overflow boxes. Does not raise the threshold at which the character becomes incompacitated, or wound modifiers from damage taken.",
    quality "Will To Live 3" 15 "The character gains 3 additional damage overflow boxes. Does not raise the threshold at which the character becomes incompacitated, or wound modifiers from damage taken.",
    -- Negative Qualities
    quality "Mild Addiction" (-5) "Experience a craving about once or twice a week. Can ignore the craving for -2 dice modifier to Willpower / Body to resist",
    quality "Moderate Addiction" (-10) "Experience a craving at least once a day. Can ignore the craving for -4 dice modifier to Willpower / Body to resist",
    quality "Severe Addiction" (-20) "Experience constant cravings, at least twice a day. Can ignore the cravig for -6 dice modifier to Willpower / Body to resist",
    quality "Burnout" (-30) "Same as Severe addiction, but you take -1 to essense, and will continue losing essense until you die or clean up",
    -- Allergies are a separate list
    Quality "Astral Beacon" (BP (-5)) "All astral signatures last twice as long, and others assensing the signature get +2 dice pool modifier" Nothing (Just ["Adept", "Magician", "Mystic Adept"]),
    quality "Bad Luck" (-20) "Whenever the character spends edge, roll 1d6. On a 1 the result is the opposite of what was intended",
    quality "Coderblock" (-5) "Receive -2 on any attempt at a specific matrix action. Only applies to matrix actions with an associated test",
    quality "Combat Paralysis" (-20) "On the first initiative pass only rolls half initiative. Receives -3 pool modifier to surprise tests, -1 modifier to composure tests in combat situations",
    quality "Elf Poser" (-5) "A human that dresses and acts like an elf. Being outed may result in hostility by Elves and Humans",
    quality "Gremlins 1" (-5) "Reduces the number of dice needed to glitch by 1 when using modern technology",
    quality "Gremlins 2" (-10) "Reduces the number of dice needed to glitch by 2 when using modern technology",
    quality "Gremlins 3" (-15) "Reduces the number of dice needed to glitch by 3 when using modern technology",
    quality "Gremlins 4" (-20) "Reduces the number of dice needed to glitch by 4 when using modern technology",
    quality "Incompetent" (-5) "Character is incompetent at a particular active skill, and will be rated as unaware in that skill. May also have to roll for tasks that competent characters wouldn't need to",
    quality "Infirm" (-20) "The cost of learning or improving Physical skill is twice normal including during character creation. Will be considered unaware for any physical tests.",
    quality "Low Pain Tolerance" (-10) "Incur -1 wound modifier for every 2 boxes of damage rather than 3",
    quality "Ork Poser" (-5) "A human or elf that dresses and acts like an Ork. May receive hostility from Orks if discovered, also Elves and Humans as a race traitor.",
    quality "Pacifist 1" (-5) "Character avoids confrontation and will only kill in self-defence.",
    quality "Pacifist 2" (-10) "Character can not kill, and if they somehow do they will be overcome with depression for several weeks",
    quality "Scorched" (-5) "Character receives a -2 dice pool modifier to any Willpower-related tests madw when facing Black IC or BTLs.",
    quality "Sensitive Neural Structure" (-5) "Character is vulterable to neural damage from BTLs, Black IC, dumpshock, etc. Get -2 to resist damage from Simsense",
    quality "Sensitive System" (-15) "Double all essense losses caused by cyberware implants, does not affect bioware",
    quality "Simsense Vertigo" (-10) "Receive -2 dice modifier to all tests when interacting with AR, VR, or Simsense",
    quality "Sinner 1" (-5) "Character has a SIN",
    quality "Sinner 2" (-10) "Character has a criminal record",
    quality "Spirit Bane" (-10) "Certain types of spirits act aggressively towards this character",
    quality "Uncouth" (-20) "The cost of learning or improving Social skills is twice normal (including character creation), and are treated as Unaware in any social skill they do not have 1 or more points in.",
    quality "Uneducated" (-20) "Considered Unaware in Technical, Academic Knowledge, and Professional Knowledge skills they do not possess. Cost for improving skills is twice normal and cannot learn skill groups",
    quality "Weak Immune System" (-5) "The character has -2 dice to any tests for resisting diseases."]]
