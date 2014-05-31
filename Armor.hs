module Armor (getArmor) where

import Equipment
import Data.Map

armor name b i = Armor name b i Body []

notFoundArmor x = Armor ("Could not find '" ++ x ++ "' in the armor database.") 0 0 Body []
getArmor x = findWithDefault (notFoundArmor x) x armorDb
armorDb = fromList [(getName x, x) | x <- [
    Clothing "Clothing" 0 0,
    Clothing "Feedback Clothing" 0 0,
    Clothing "Leather Jacket" 2 2,
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
    where getName (Armor name _ _ _ _) = name
          getName (Clothing name _ _) = name  


