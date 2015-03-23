module Spells (
    SpellType(..), 
    SpellRange(..), 
    SpellDuration(..), 
    Spell(..),
    spellDb,
    getSpell)
where

import Data.Map (Map, fromList, findWithDefault)

data SpellType = Physical | Mana deriving (Show)
data SpellRange = LoS | LoSArea | Touch | Voluntary | Area  deriving (Show)
data SpellDamage = Sd_Physical | Sd_Stun deriving (Show)
data SpellDuration = Instant | Sustained | Permanent deriving (Show)
data DV = DV String deriving (Show)
data CombatSpellType = Cst_Direct | Cst_Indirect deriving (Show)
data ElementalType = Fire | Lightning | Acid | None deriving (Show)
data DetectionSpellType = Dst_Active | Dst_Passive deriving (Show)
data DetectionSpellFocus = Dst_Directional | Dst_Area | Dst_Psychic deriving (Show)
data IllusionSpellType = Ist_Realistic | Ist_Obvious deriving (Show)
data IllusionSpellSenses = Ist_Single | Ist_Multi deriving (Show)
data ManipulationSpellType = Mst_Mental | Mst_Physical | Mst_Environmental deriving (Show)

data Spell = 
        CombatSpell String SpellType SpellRange SpellDamage SpellDuration DV String CombatSpellType ElementalType |
        DetectionSpell String SpellType SpellRange SpellDuration DV String DetectionSpellType DetectionSpellFocus |
        Spell String SpellType SpellRange SpellDuration DV String |
        IllusionSpell String SpellType SpellRange SpellDuration DV String IllusionSpellType IllusionSpellSenses |
        ManipulationSpell String SpellType SpellRange SpellDuration DV String ManipulationSpellType deriving (Show)


spellName :: Spell -> String    
spellName (CombatSpell name _ _ _ _ _ _ _ _ ) = name
spellName (DetectionSpell name _ _ _ _ _ _ _) = name
spellName (Spell name _ _ _ _ _) = name
spellName (IllusionSpell name _ _ _ _ _ _ _) = name
spellName (ManipulationSpell name _ _ _ _ _ _) = name

notFoundSpell :: String -> Spell
notFoundSpell x = Spell ("Could not find '" ++ x ++ "' in spell database") Mana LoS Instant (DV "") ""

getSpell :: String -> Spell
getSpell x = findWithDefault (notFoundSpell x) x spellDb

spellDb :: Map String Spell
spellDb = fromList [(spellName x, x) | x <- [
    CombatSpell "Acid Stream" Physical LoS Sd_Physical Instant (DV "F/2+3") "" Cst_Indirect Acid,
    CombatSpell "Toxic Wave" Physical LoSArea Sd_Physical Instant (DV "F/2+5") "" Cst_Indirect Acid,
    CombatSpell "Punch" Physical Touch Sd_Stun Instant (DV "F/2-2") "" Cst_Indirect None,
    CombatSpell "Clout" Physical LoS Sd_Stun Instant (DV "F/2") "" Cst_Indirect None,
    CombatSpell "Blast" Physical LoSArea Sd_Stun Instant (DV "F/2+2") "" Cst_Indirect None,
    CombatSpell "Death Touch" Mana Touch Sd_Physical Instant (DV "F/2-2") "" Cst_Direct None,
    CombatSpell "Manabolt" Mana LoSArea Sd_Physical Instant (DV "F/2") "" Cst_Direct None,
    CombatSpell "Manaball" Mana LoSArea Sd_Physical Instant (DV "F/2+2") "" Cst_Direct None,
    CombatSpell "Flamethrower" Physical LoS Sd_Physical Instant (DV "F/2+3") "" Cst_Indirect Fire,
    CombatSpell "Fireball" Physical LoSArea Sd_Physical Instant (DV "F/2+5") "" Cst_Indirect Fire,
    CombatSpell "Lightning Bolt" Physical LoS Sd_Physical Instant (DV "F/2+3") "" Cst_Indirect Lightning,
    CombatSpell "Ball Lightning" Physical LoSArea Sd_Physical Instant (DV "F/2+5") "" Cst_Indirect Lightning,
    CombatSpell "Shatter" Physical Touch Sd_Physical Instant (DV "F/2-1") "" Cst_Direct None,
    CombatSpell "Powerbolt" Physical LoS Sd_Physical Instant (DV "F/2+1") "" Cst_Direct None,
    CombatSpell "Powerball" Physical LoSArea Sd_Physical Instant (DV "F/2+3") "" Cst_Direct None,
    CombatSpell "Knockout" Mana Touch Sd_Stun Instant (DV "F/2-3") "" Cst_Direct None,
    CombatSpell "Stunbolt" Mana LoS Sd_Stun Instant (DV "F/2-1") "" Cst_Direct None,
    CombatSpell "Stunball" Mana LoSArea Sd_Stun Instant (DV "F/2+1") "" Cst_Direct None]]
