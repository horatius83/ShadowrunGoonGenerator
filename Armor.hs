module Armor (getArmor) where

import Equipment (Equipment (Armor), ArmorType(..), ArmorMod(..), armorName)
import Data.Map

armor :: String -> Int -> Int -> Equipment 
armor name b i = Armor name b i Body []

notFoundArmor :: String -> Equipment 
notFoundArmor x = armor ("Could not find '" ++ x ++ "' in the armor database.") 0 0 

getArmor :: String -> Equipment 
getArmor x = findWithDefault (notFoundArmor x) x armorDb

armorDb :: Map String Equipment 
armorDb = fromList [(armorName x, x) | x <- [
    armor "Clothing" 0 0, 
    armor "Feedback Clothing" 0 0, 
    armor "Leather Jacket" 2 2,
    armor "Actioneer Business Clothes" 5 3,
    armor "Armor Clothing" 4 0,
    armor "Armor Vest" 6 4,
    armor "Armor Jacket" 8 6,
    armor "Camouflage Suit" 8 6,
    armor "Chameleon Suit" 6 4,
    armor "Full Body Armor" 10 6,
    Armor "Heavy Helmet" 2 2 Helmet [],
    armor "Lined Coat" 6 4,
    armor "Urban Explorer Jumpsuit" 6 6,
    Armor "Light Helmet" 0 2 Helmet [],
    Armor "Medium Helmet" 1 2 Helmet [],
    Armor "Ballistic Shield" 6 4 Shield [],
    Armor "Riot Shield" 2 6 Shield [],
    Armor "Taser Shield" 2 6 Shield [Nonconductivity]]]
