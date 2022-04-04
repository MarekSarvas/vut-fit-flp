-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022
-- Module: Implementation of algorithm for removing simple rules form Grammar

module RemoveSimpleRules (removeSimple) where

import Data.List (nub)

import Types (Nonterminals, Terminals, Rules, Grammar(..), NaSet, NaSets)  

-- ======================= Algorithm 1 =======================
-- remove simple rules and create updated Grammar
removeSimple :: Grammar -> Grammar
removeSimple g = Grammar {nonterminals=nonterminals g, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
    where newRules = createRules (createNa (nonterminals g) (getSimpleRules (rules g) (nonterminals g))) (removeSimpleRules (rules g) ( terminals g))
 

-- creates list of N_A sets for each nonterminal A and concat them together e.g.: [(A, N_A), (B, N_B)]
createNa :: Nonterminals -> Rules -> NaSets
createNa [] _ = []
createNa (x:xs) rs =  (x, createNa' rs [x]) : createNa xs rs

-- create one N_A set, recursively until it does not change, algorithm 4.5 (1)  
createNa' ::  Rules -> [String] -> Nonterminals 
createNa' rs nA0 =
    if nA0 == nA1 then nA1
    else createNa' rs nA1
        where nA1 = nub (nA0 ++ concatMap snd (filter (\r -> fst r `elem` nA0) rs )) -- TODO change for union ? 

-- recursively add new rules while removing simple rules
createRules :: NaSets -> Rules -> Rules
createRules [] _ = []
createRules (x:xs) rs = createRules' x rs ++ createRules xs rs 

-- create new rules such as from A->B B->ac creates  A->ac
-- if left side of the rule is not in N_A set it creates empty rule and then it is filtered out
createRules' :: NaSet -> Rules -> Rules
createRules' nas rs = filter (\r -> fst r /= "") (map (\r -> if fst r `elem` snd nas then (fst nas, snd r) else ("",[])) rs)

-- get list of simple rules A->B
getSimpleRules :: Rules -> Nonterminals -> Rules
getSimpleRules rs nonts = filter ( \r -> any (`elem` nonts) (snd r) && length (snd r) == 1) rs

-- get list of rules witout simple rules
removeSimpleRules :: Rules -> Terminals -> Rules
removeSimpleRules rs ts = filter ( \r -> any (`elem` ts) (snd r) || length (snd r) /= 1) rs

