-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022

module CNF (toCNF) where

import System.Environment   
import Data.List (nub, intercalate, union)

import Types (Nonterminals, Nonterminal, Terminal, Terminals, Rule, Rules, Grammar(..))  

-- ===================== Algorithm 2 =======================
-- transform Grammar to CNF
toCNF :: Grammar -> Grammar 
toCNF g = Grammar {nonterminals=newNonterms, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
    where
        newRules = createCNFRules (rules g)  (terminals g) -- create rules in form of A->a or A->BC
        newNonterms = nonterminals g `union` map fst newRules -- add nonterminals from rules into nonterminal set  

-- if some rule that is not in CNF exists, recursively transform it and get rid of duplicates with 'nub'
createCNFRules :: Rules -> Terminals -> Rules
createCNFRules rs terms  =
    if all (`isNormRule` terms) rs then nub rs 
    else createCNFRules (nub $ concatMap (\r -> if not (isNormRule r terms) then uncurry transformRule' r terms else [r]) rs) terms 

-- Checks if current rule is in CNF A->BC or  A->a
isNormRule :: Rule -> Terminals -> Bool
isNormRule (_, r) terms = length r == 2 && all (`notElem` terms ) r || length r == 1 && all (`elem` terms) r 

-- create rules that in CNF from rule that is not in CNF
transformRule' :: Nonterminal -> Nonterminals -> Terminals -> Rules
transformRule' l r terms -- l is left side of rule, r right side of the rule 
    |  length r > 2 && startWithTerm r =  (l, (head r++"'") : [createNewNont $ tail r])  : (head r++"'", [head r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->aBC create A->a'<BC>, a'->a and recursively call itself
    |  length r > 2 && not (startWithTerm r) =  (l, head r : [createNewNont $ tail r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->DaBC create A->D<aBC> and recursively call itself
    -- | l `elem` terms && length r > 2 && not (startWithTerm r) =  (l++"'", head r : [createNewNont $ tail r]) : (l++"'", [l]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->DaBC create A->D<aBC> and recursively call itself
    -- | l `elem` terms && length r > 2 && startWithTerm r =  (l++"'", (head r++"'") : [createNewNont $ tail r]): (l++"'", [l]) : (head r++"'", [head r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->aBC create A->a'<BC>, a'->a and recursively call itself
    | l `elem` terms && length r == 2 = (l++"'", transformTerms r) : (l++"'", [l]) : concatMap (\r' -> if r' `elem` terms then [(r'++"'", [r'])] else [] ) r
    | length r == 2 = [(l, transformTerms r)] ++ concatMap (\r' -> if r' `elem` terms then [(r'++"'", [r'])] else [] ) r
    | otherwise = []
        where 
            startWithTerm x = head x `elem` terms -- return Bool if right side of rule starts with terminal symbol
            createNewNont x = intercalate "" (["<"] ++ x ++ [">"]) -- create new nonterminal according to algorithm e.g. ["A", "a", "C"] -> "<AaC>" 
            transformTerms x = map (\y -> if y `elem` terms then y++"'" else y) x -- create nonterminal from terminal or no change if nonterminal is present

