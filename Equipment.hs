module Equipment (
    Equipment(Armor, Equipment), 
    ArmorMod (ChemicalProtection, ChemicalSeal, FireResistance, Insulation, Nonconductivity, ShockFrills, ThermalDamping), 
    ArmorType (Body, Helmet, Shield), 
    armorName,
    getEquipment
) where

import Data.Map (Map, fromList, findWithDefault)

data ArmorMod = ChemicalProtection 
            | ChemicalSeal 
            | FireResistance 
            | Insulation 
            | Nonconductivity
            | ShockFrills
            | ThermalDamping deriving (Show)

data ArmorType = Body | Helmet | Shield deriving (Show)    

data Equipment = 
    Armor { 
        armorName :: String, 
        armorBallistic :: Int, 
        armorImpact :: Int,
        armorType :: ArmorType,
        armorMods :: [ArmorMod]} 
    | Commlink { clModel :: String, clResponse :: Int, clSignal :: Int}
    | OS { osName :: String, osFirewall :: Int, osSystem :: Int}
    | Accessory { accessoryName :: String, accessoryDevice :: Int }
    | Equipment String deriving (Show)

getEquipment :: String -> Equipment
getEquipment name = findWithDefault notFoundEquipment name equipmentDb 
    where
        notFoundEquipment = Equipment $ name ++ " was not found."

equipmentDb :: Map String Equipment
equipmentDb = fromList $ commlinks ++ oss ++ accessories
    where
        commlinks =  [(model, Commlink model response signal) | (model, response, signal) <- [
                        ("MetaLink", 1, 2),
                        ("CMT Clip", 1, 3),
                        ("Sony Emperor", 2, 3),
                        ("Renraku Sensei", 2, 4),
                        ("Novatech Airware", 3, 3),
                        ("Erika Elite", 3, 4),
                        ("Hermes Ikon", 4, 3),
                        ("Transys Avalon", 4, 5)]]
        oss = [(name, OS name firewall system) | (name, firewall, system) <- [
                        ("Vector Xim", 1, 1), 
                        ("Redcap NIX", 1, 2), 
                        ("Renraku Ichi", 2, 2), 
                        ("Mangadyne Deva", 3, 3), 
                        ("Iris Orb", 3, 3), 
                        ("Novatech Navi", 3, 4)]]
        accessories =  [(name, Accessory name device) | (name, device) <- [
                        ("AR Gloves", 3), 
                        ("Biometric Reader", 3), 
                        ("Nanopaste Trodes", 3), 
                        ("Printer", 1), 
                        ("Satellite Link", 3),
                        ("Sim Module", 3),
                        ("Modified for BTL/hot sim", 3),
                        ("Simrig", 3),
                        ("Skinlink", 3),
                        ("Subvocal Microphone", 3),
                        ("Trodes", 3)]]

