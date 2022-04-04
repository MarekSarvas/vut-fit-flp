-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022

module Main (main) where

import System.Environment   
--import Text.Parsec (char, endBy, eof, many1, newline, oneOf, parse, ParseError, sepBy1, string)
--import Text.Parsec.String (Parser)
--import Data.List (nub, intercalate, union, sort)
import Text.Parsec (ParseError)
import Data.List (nub)
import Types (Nonterminals, Terminals, Rules, Grammar(..))  
import Parser (parseGrammar)
import RemoveSimpleRules (removeSimple)
import CNF (toCNF)
-- Data types for Grammar representation
-- type Nonterminal = String
-- type Nonterminals = [Nonterminal]
-- 
-- type Terminal = String
-- type Terminals = [Terminal]
-- 
-- -- Nonterminal is string because of e.g. <Aa>
-- type Rule = (Nonterminal, [String])
-- type Rules = [Rule] 
-- 
-- -- sets for algorithm 1, for removing simple rules
-- type NaSet = (Nonterminal, Nonterminals)
-- type NaSets = [NaSet]
-- 
-- -- grammar data type
-- data Grammar = Grammar
--     { nonterminals :: Nonterminals,
--       terminals :: Terminals,
--       startNonterm :: Nonterminal,
--       rules :: [Rule]
--     } 
-- instance Show Grammar where
--     show (Grammar n t s rs) = remnewline $ unlines $ [intercalate "," n] ++ [intercalate "," t] ++ [s] ++
--         sort (map (\r -> fst r ++ "->" ++ intercalate "" (snd r)) rs)
--             where remnewline = reverse . dropWhile (=='\n') . reverse -- remove last \n so empty line does not appear
--     --
-- ============= PARSER ====================
-- Parse correct Nonterminal input 
-- parseNonTerminals :: Parser Nonterminals 
-- parseNonTerminals = sepBy1 parseOneNonTerminal (char ',') <* newline --read until \n
-- 
-- parseOneNonTerminal :: Parser Nonterminal 
-- parseOneNonTerminal = fmap (\x -> [x]) (oneOf ['A'..'Z']) 
-- 
-- -- Parse correct Terminal input 
-- parseTerminals :: Parser Terminals 
-- parseTerminals = sepBy1 parseOneTerminal (char ',') <* newline --read until \n 
-- 
-- parseOneTerminal :: Parser Terminal 
-- parseOneTerminal = fmap (\x -> [x]) (oneOf ['a'..'z']) 
-- 
-- -- parse starting symbol
-- parseStartSymbol :: Parser String 
-- parseStartSymbol =  fmap (\x -> [x]) (oneOf ['A'..'Z']) <* newline
-- 
-- -- parse rules 
-- parseRules :: Parser Rules
-- parseRules =  endBy parseRule newline 
-- 
-- parseRule :: Parser Rule 
-- parseRule = toRule <$> parseLeft <*> parseRight
--     where toRule l r = (l, map (\x -> [x]) r)
--  
-- -- parse left side of the rule
-- parseLeft :: Parser String 
-- parseLeft = fmap (: []) (oneOf ['A'..'Z']) <* string "->" 
-- 
-- -- parse right side od the rule
-- parseRight :: Parser String 
-- parseRight = many1 (oneOf (['A'..'Z']++['a'..'z']))

-- parse whole content to Grammar inner representation
-- parseGrammar :: String -> Either ParseError Grammar
-- parseGrammar = parse parseGrammar' "Wrong input grammar"

--parseGrammar' :: Parser Grammar 
--parseGrammar'  = do
--    nonterms <- parseNonTerminals 
--    terms <- parseTerminals  
--    start <- parseStartSymbol
--    rule <- parseRules <* eof
--    return Grammar {nonterminals=nonterms, terminals=terms, startNonterm=start, rules=rule}

-- check for correct starting symbol and valid terminals/nonterminals in rules 
checkSemantics :: Grammar -> Grammar
checkSemantics g = 
    if startNonterm g `notElem` nonterminals g then error "Invalid starting symbol"
    else if not $ checkRules (rules g) (terminals g) (nonterminals g) then error "Invalid symbol in rules"
    else if hasDuplicates (nonterminals g) || hasDuplicates (terminals g) || hasDuplicates (rules g) then error "Duplicate symbols or rules in grammar"  -- check for duplicate symbols
    else g 
        where hasDuplicates x = length x /= length (nub x)

checkRules :: Rules -> Terminals -> Nonterminals -> Bool
checkRules [] _ _ = True
checkRules (x:xs) terms nonterms = fst x `elem` nonterms && all (\y -> y `elem` (terms++nonterms)) (snd x) && checkRules xs terms nonterms

---- ======================= Algorithm 1 =======================
--removeSimple :: Grammar -> Grammar
--removeSimple g = Grammar {nonterminals=nonterminals g, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
--    where newRules = createRules (createNa (nonterminals g) (getSimpleRules (rules g) (nonterminals g))) (removeSimpleRules (rules g) ( terminals g))
-- 
--
---- creates list of N_A sets for each nonterminal A and concat them together
--createNa :: Nonterminals -> Rules -> NaSets
--createNa [] _ = []
--createNa (x:xs) rs =  (x, createNa' rs [x]) : createNa xs rs
--
---- create one N_A set, recursively until it does not change, algorithm 4.5 (1)  
--createNa' ::  Rules -> [String] -> Nonterminals 
--createNa' rs nA0 =
--    if nA0 == nA1 then nA1
--    else createNa' rs nA1
--        where nA1 = nub (nA0 ++ concatMap snd (filter (\r -> fst r `elem` nA0) rs )) -- TODO change for union ? 
--
---- recursively add new rules while removing simple rules
--createRules :: NaSets -> Rules -> Rules
--createRules [] _ = []
--createRules (x:xs) rs = createRules' x rs ++ createRules xs rs 
--
---- create new rules such as from A->B B->ac creates  A->ac
---- if left side of the rule is not in N_A set it creates empty rule and then it is filtered out
--createRules' :: NaSet -> Rules -> Rules
--createRules' nas rs = filter (\r -> fst r /= "") (map (\r -> if fst r `elem` snd nas then (fst nas, snd r) else ("",[])) rs)
--
---- get list of simple rules A->B
--getSimpleRules :: Rules -> Nonterminals -> Rules
--getSimpleRules rs nonts = filter ( \r -> any (`elem` nonts) (snd r) && length (snd r) == 1) rs
--
---- get list of rules witout simple rules
--removeSimpleRules :: Rules -> Terminals -> Rules
--removeSimpleRules rs ts = filter ( \r -> any (`elem` ts) (snd r) || length (snd r) /= 1) rs
--
-- ===================== Algorithm 2 =======================
-- transform Grammar to CNF
--toCNF :: Grammar -> Grammar 
--toCNF g = Grammar {nonterminals=newNonterms, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
--    where
--        newRules = createCNFRules (rules g)  (terminals g) -- create rules in form of A->a or A->BC
--        newNonterms = nonterminals g `union` map fst newRules -- add nonterminals from rules into nonterminal set  
--
---- if some rule that is not in CNF exists, recursively transform it and get rid of duplicates with 'nub'
--createCNFRules :: Rules -> Terminals -> Rules
--createCNFRules rs terms  =
--    if all (`isNormRule` terms) rs then nub rs 
--    else createCNFRules (nub $ concatMap (\r -> if not (isNormRule r terms) then uncurry transformRule' r terms else [r]) rs) terms 
--
---- Checks if current rule is in CNF A->BC or  A->a
--isNormRule :: Rule -> Terminals -> Bool
--isNormRule (_, r) terms = length r == 2 && all (`notElem` terms ) r || length r == 1 && all (`elem` terms) r 
--
---- create rules that in CNF from rule that is not in CNF
--transformRule' :: Nonterminal -> Nonterminals -> Terminals -> Rules
--transformRule' l r terms -- l is left side of rule, r right side of the rule 
--    |  length r > 2 && startWithTerm r =  (l, (head r++"'") : [createNewNont $ tail r])  : (head r++"'", [head r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->aBC create A->a'<BC>, a'->a and recursively call itself
--    |  length r > 2 && not (startWithTerm r) =  (l, head r : [createNewNont $ tail r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->DaBC create A->D<aBC> and recursively call itself
--    -- | l `elem` terms && length r > 2 && not (startWithTerm r) =  (l++"'", head r : [createNewNont $ tail r]) : (l++"'", [l]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->DaBC create A->D<aBC> and recursively call itself
--    -- | l `elem` terms && length r > 2 && startWithTerm r =  (l++"'", (head r++"'") : [createNewNont $ tail r]): (l++"'", [l]) : (head r++"'", [head r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->aBC create A->a'<BC>, a'->a and recursively call itself
--    | l `elem` terms && length r == 2 = (l++"'", transformTerms r) : (l++"'", [l]) : concatMap (\r' -> if r' `elem` terms then [(r'++"'", [r'])] else [] ) r
--    | length r == 2 = [(l, transformTerms r)] ++ concatMap (\r' -> if r' `elem` terms then [(r'++"'", [r'])] else [] ) r
--    | otherwise = []
--        where 
--            startWithTerm x = head x `elem` terms -- return Bool if right side of rule starts with terminal symbol
--            createNewNont x = intercalate "" (["<"] ++ x ++ [">"]) -- create new nonterminal according to algorithm e.g. ["A", "a", "C"] -> "<AaC>" 
--            transformTerms x = map (\y -> if y `elem` terms then y++"'" else y) x -- create nonterminal from terminal or no change if nonterminal is present
--
-- ===================== Load Grammar =======================
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
