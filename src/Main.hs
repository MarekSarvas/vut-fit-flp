-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022

module Main (main) where

import System.Environment   
import Text.Parsec (ParseError)
import Data.List (nub)
import Types (Nonterminals, Terminals, Rules, Grammar(..))  
import Parser (parseGrammar)
import RemoveSimpleRules (removeSimple)
import CNF (toCNF)

-- check for correct starting symbol and valid terminals/nonterminals in rules 
checkSemantics :: Grammar -> Grammar
checkSemantics g = 
    if startNonterm g `notElem` nonterminals g then error "Invalid starting symbol"
    else if not $ checkRules (rules g) (terminals g) (nonterminals g) then error "Invalid symbol in rules"
    else if hasDuplicates (nonterminals g) || hasDuplicates (terminals g) || hasDuplicates (rules g) then error "Duplicate symbols or rules in grammar"  -- check for duplicate symbols
    else if not startInRules then error "Start symbol is not present on left side of any rule."
    else g 
        where 
            hasDuplicates x = length x /= length (nub x)
            startInRules = any (\r -> fst r == startNonterm g) (rules g)

checkRules :: Rules -> Terminals -> Nonterminals -> Bool
checkRules [] _ _ = True
checkRules (x:xs) terms nonterms = fst x `elem` nonterms && all (\y -> y `elem` (terms++nonterms)) (snd x) && checkRules xs terms nonterms

-- Load Grammar
main :: IO ()
main = do 
    (action, content) <- parseArgs <$> getArgs
    string_g <- content  
    g <-  performAction action string_g 
    putStrLn g
    return ()

-- parse programme arguments and return them with grammar as content 
parseArgs :: [String] -> (String, IO String) 
parseArgs [] = error "No arguments" 
parseArgs [x] 
    | x `elem` ["-i", "1", "2"] = (x, getContents)
    | otherwise = error "Wrong first argument"
parseArgs (x:y:_) 
    | x `elem` ["-i", "1", "2"] = (x, readFile y)
    | otherwise = error "Wrong first argument"

-- checks for parse error, remove Either
checkError :: Either ParseError Grammar -> Grammar
checkError x = case x of
    (Right g) -> g
    (Left _) -> error "Invalid grammar"

-- preform desired action based on program arguments
performAction :: String -> String -> IO String 
performAction action content
    | action == "-i" = return $ show $ checkError $ fmap checkSemantics ( parseGrammar content )
    | action == "1" = return $ show $ checkError (removeSimple <$> fmap checkSemantics (parseGrammar content))
    | action == "2" = return $ show $ checkError (toCNF . removeSimple <$> fmap checkSemantics (parseGrammar content))   
    | otherwise = error "Wrong argument" 
