-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022
-- Module: Custom data types

module Types (Grammar(..), Nonterminals, Nonterminal, Terminal, Terminals, Rule, Rules, NaSet, NaSets) where

import Data.List (intercalate, sort)

-- Data types for Grammar representation
-- Nonterminal is string because of e.g. <Aa>
type Nonterminal = String
type Nonterminals = [Nonterminal]

type Terminal = String
type Terminals = [Terminal]

-- rules stored as (A, ["A", "b", "C"]) for A->AbC
type Rule = (Nonterminal, [String])
type Rules = [Rule] 

-- sets for algorithm 1, for removing simple rules
type NaSet = (Nonterminal, Nonterminals)
type NaSets = [NaSet]

-- grammar data type
data Grammar = Grammar
    { nonterminals :: Nonterminals,
      terminals :: Terminals,
      startNonterm :: Nonterminal,
      rules :: Rules 
    } 
instance Show Grammar where
    show (Grammar n t s rs) = remnewline $ unlines $ [intercalate "," n] ++ [intercalate "," t] ++ [s] ++
        sort (map (\r -> fst r ++ "->" ++ intercalate "" (snd r)) rs)
            where remnewline = reverse . dropWhile (=='\n') . reverse -- remove last \n so empty line does not appear
