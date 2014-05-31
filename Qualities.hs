module Qualities where

import Data.Map
import Data.List

data Quality = Quality { 
    qualityName :: String,
    qualityBp :: Int,
    qualityDescription :: String,
    qualityIncompatible :: Maybe [String],
    qualityDependency :: Maybe [String]} deriving (Show)

quality name bp description = Quality name bp description Nothing Nothing

qualityDb = fromList [(qualityName quality, quality) | quality <- [
    Quality "Adept" 5 "Adept characters channel magic through their bodies, Magic value starts at 1, can be increased up to 6. Power points equal Magic Value" (Just ["Magician", "Mystic Adept", "Technomancer"]) Nothing,
    quality "Ambidexterous" 5 "Character does not suffer modifiers for using an off-hand weapon",
    quality "Animal Empathy" 10 "+2 dice pool modifier for all tests involving the influence or control of an animal. Does not work on sentient creatures.",
    quality "Aptitude" 10 "Characters may improve one skill above its' natural value to 7. Increasing beyond 6 costs double the normal Karma cost or double the BP cost.",
    Quality "Astral Chameleon" 5 "All astral signatures left by the character last half as long, assensing this character takes a -2 dice pool modifier" Nothing (Just ["Adept", "Magician", "Mystic Adept"]),
    quality "Blandness" 10 "Individuals attempting to shadow / find this character physically though social means or in a crowded area receive a -2 dice pool modifier",
    quality "Codeslinger" 10 "Adept at performing a particular matrix action. Receive +2 dice pool for that action",
    quality "Double Jointed" 5 "+2 dice pool modifier for Escape Artist Tests. Maybe be able squeeze into small spaces.",
    quality "Erased 1" 5 "Criminal SINs and unwanted data disappear within a week.",
    quality "Erased 2" 10 "Any SIN, undesireable information is burnt after 24 hours",
    quality "Exceptional Attribute" 20 "Increases the maximum for a particular attribute",
    quality "First Impression" 5 "+2 dice pool when infiltrating organizations, making contacts, or generally meeting people for the first time",
    Quality "Focused Concentration 1" 10 "+1 dice pool modifier for all drain tests" Nothing (Just ["Magician", "Mystic Adept"]),
    Quality "Focused Concentration 2" 20 "+2 dice pool modifier for all drain tests" Nothing (Just ["Magician", "Mystic Adept"]),
    quality "Guts" 5 "+2 dice pool on tests made to resist fear and intimidation (including magically induced terror)",
    Quality "High Pain Tolerance 1" 5 "Ignore one box of damage when calculating wound modifiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    Quality "High Pain Tolerance 2" 10 "Ignore two boxes of damage when calculating wound modifiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    Quality "High Pain Tolerance 3" 15 "Ignore three boxes of damage when calculating wound mofidiers" (Just ["Pain Resistance", "Pain Editor", "Damage Compensator"]) Nothing,
    quality "Home Ground" 10 "+2 dice pool for all Active Skills where tests are made in this characters home turf",
    quality "Human-Looking" 5 "This metahuman character can 'pass' as a human. Only elves, dwarfs and orks can take this quality",
    quality "Lucky" 20 "Increases maximum edge by 1",
    Quality "Magician" 20 "Can cast spells, summon spirits etc." (Just ["Adept", "Mystic Adept", "Technomancer"]) Nothing,
    quality "Magic Resitance 1" 5 "Receive one additional die for magic resistance tests (this applies to healing as well)",
    quality "Magic Resitance 2" 10 "Receive two additional dice for magic resistance tests (this applies to healing as well)",
    quality "Magic Resistance 3" 15 "Receive three additional dice for magic resistance tests (this applies to healing as well)",
    quality "Magic Resistance 4" 20 "Receive four additional dice for magic resistance tests (this applies to healing as well)",
    Quality "Mentor Spirit" 5 "This character has a patron mentor spirit" Nothing (Just ["Magician", "Adept", "Mystic Adept"]),
    quality "Murky Link" 10 "Any ritual sorcery directed against this character receives a -3 dice pool modifier."]]
