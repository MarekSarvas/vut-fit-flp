import System.Environment   
import Text.Parsec (char, endBy, eof, many1, newline, oneOf, parse, ParseError, sepBy1, string)
import Text.Parsec.String (Parser)
import Data.List (nub, intercalate, union, sort)
import Data.Either (fromRight)

-- Data types for Grammar representation
type Nonterminal = String
type Nonterminals = [Nonterminal]

type Terminal = String
type Terminals = [Terminal]

-- Nonterminal is string because of e.g. <Aa>
type Rule = (Nonterminal, [String])
type Rules = [Rule] 

type NaSet = (Nonterminal, Nonterminals)
type NaSets = [NaSet]


data Grammar = Grammar
    { nonterminals :: Nonterminals,
      terminals :: Terminals,
      startNonterm :: Nonterminal,
      rules :: [Rule]
    } 
instance Show Grammar where
    show (Grammar n t s rs) = remnewline $ unlines $ [intercalate "," n] ++ [intercalate "," t] ++ [s] ++
        sort (map (\r -> fst r ++ "->" ++ intercalate "" (snd r)) rs)
            where remnewline = reverse . dropWhile (=='\n') . reverse
    --
-- ============= PARSER ====================
-- Parse correct Nonterminal input 
parseNonTerminals :: Parser Nonterminals 
parseNonTerminals = sepBy1 parseOneNonTerminal (char ',') <* newline --read until \n

parseOneNonTerminal :: Parser Nonterminal 
parseOneNonTerminal = fmap (\x -> [x]) (oneOf ['A'..'Z']) 

-- Parse correct Terminal input 
parseTerminals :: Parser Terminals 
parseTerminals = sepBy1 parseOneTerminal (char ',') <* newline --read until \n 

parseOneTerminal :: Parser Terminal 
parseOneTerminal = fmap (\x -> [x]) (oneOf ['a'..'z']) 

parseStartSymbol :: Parser String 
parseStartSymbol =  fmap (\x -> [x]) (oneOf ['A'..'Z']) <* newline

parseRules :: Parser Rules
parseRules =  endBy parseRule newline 

parseRule :: Parser Rule 
parseRule = toRule <$> parseLeft <*> parseRight
    where toRule l r = (l, map (\x -> [x]) r)

parseLeft :: Parser String 
parseLeft = fmap (: []) (oneOf ['A'..'Z']) <* string "->" 

parseRight :: Parser String 
parseRight = many1 (oneOf (['A'..'Z']++['a'..'z']))

parseGrammar :: String -> Either ParseError Grammar
parseGrammar = parse parseGrammar' "Wrong Grammar Format"

parseGrammar' :: Parser Grammar 
parseGrammar'  = do
    nonterms <- parseNonTerminals 
    terms <- parseTerminals  
    start <- parseStartSymbol
    rule <- parseRules <* eof
    return Grammar {nonterminals=nonterms, terminals=terms, startNonterm=start, rules=rule}

-- foldl pre rules a vo funkcii && ci je elementom nonterms ++ terms 
checkSemantics :: Grammar -> Grammar
checkSemantics g = 
    if startNonterm g `notElem` nonterminals g then error "Invalid starting symbol"
    else g 

checkRules :: Rules -> Bool
checkRules [] = True
checkRules (x:xs) = checkRules [x]

-- ======================= Algorithm 1 =======================
removeSimple :: Grammar -> Grammar
removeSimple g = Grammar {nonterminals=nonterminals g, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
    where newRules = createRules (createNa (nonterminals g) (getSimpleRules (rules g) (nonterminals g))) (removeSimpleRules (rules g) ( terminals g))
 

-- creates list of N_A sets for each nonterminal A
createNa :: Nonterminals -> Rules -> NaSets
createNa [] _ = []
createNa (x:xs) rs =  (x, createNa' rs [x]) : createNa xs rs

-- create one N_A set 
createNa' ::  Rules -> [String] -> Nonterminals 
createNa' rs nA0 =
    if nA0 == nA1 then nA1
    else createNa' rs nA1
        where nA1 = nub (nA0 ++ concatMap snd (filter (\r -> fst r `elem` nA0) rs )) -- TODO change for union ? 


createRules :: NaSets -> Rules -> Rules
createRules [] _ = []
createRules (x:xs) rs = createRules' x rs ++ createRules xs rs 

createRules' :: NaSet -> Rules -> Rules
createRules' nas rs = filter (\r -> fst r /= "") (map (\r -> if fst r `elem` snd nas then (fst nas, snd r) else ("",[])) rs)

-- get list of simple rules A->B
getSimpleRules :: Rules -> Nonterminals -> Rules
getSimpleRules rs nonts = filter ( \r -> any (`elem` nonts) (snd r) && length (snd r) == 1) rs

removeSimpleRules :: Rules -> Terminals -> Rules
removeSimpleRules rs ts = filter ( \r -> any (`elem` ts) (snd r) || length (snd r) /= 1) rs
-- ===================== Algorithm 2 =======================

toCNF :: Grammar -> Grammar 
toCNF g = Grammar {nonterminals=newNonterms, terminals=terminals g, startNonterm=startNonterm g, rules=newRules}
    where
        newRules = createCNFRules (rules g)  (terminals g)
        newNonterms = nonterminals g `union` map fst newRules 

createCNFRules :: Rules -> Terminals -> Rules
createCNFRules rs terms  =
    if all (`isNormRule` terms) rs then nub rs 
    else createCNFRules (nub $ concatMap (\r -> if not (isNormRule r terms) then transformRule' (fst r) (snd r) terms else [r]) rs) terms 

-- Checks if current rule is in CNF A->BC or  A->a
isNormRule :: Rule -> Terminals -> Bool
isNormRule (_, r) terms = length r == 2 && all (`notElem` terms ) r || length r == 1 && all (`elem` terms) r 

-- transformRule :: Rule -> Rules
-- transformRule (l, r) = [ (l, [intercalate "" (["<"] ++ r ++ [">"])]) ] ++ []

transformRule' :: Nonterminal -> Nonterminals -> Terminals -> Rules
transformRule' l r terms 
    | l `notElem` terms && length r > 2 && startWithTerm r =  (l, (head r++"'") : [createNewNont $ tail r])  : (head r++"'", [head r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->aBC create A->a'<BC>, a'->a and recursively call itself
    | l `notElem` terms && length r > 2 && not (startWithTerm r) =  (l, head r : [createNewNont $ tail r]) : transformRule' (createNewNont $ tail r) (tail r) terms -- rules such as A->DaBC create A->D<aBC> and recursively call itself
    | length r == 2 = [(l, transformTerms r)] ++ concatMap (\r' -> if r' `elem` terms then [(r'++"'", [r'])] else [] ) r
    | otherwise = []
        where 
            startWithTerm x = head x `elem` terms
            createNewNont x = intercalate "" (["<"] ++ x ++ [">"]) 
            transformTerms x = map (\y -> if y `elem` terms then y++"'" else y) x

-- ===================== Load Grammar =======================
main :: IO ()
main = do 
    (action, content) <- parseArgs <$> getArgs
    string_g <- content  
    g <-  performAction action string_g 
    putStrLn g
    return ()


parseArgs :: [String] -> (String, IO String) 
parseArgs [] = error "No arguments" 
parseArgs [x] 
    | x `elem` ["-i", "1", "2"] = (x, getContents)
    | otherwise = error "Wrong first argument"
parseArgs (x:y:_) 
    | x `elem` ["-i", "1", "2"] = (x, readFile y)
    | otherwise = error "Wrong first argument"

performAction :: String -> String -> IO String 
performAction action content
    | action == "-i" = return $ show $ fromRight Grammar {nonterminals=[], terminals=[], startNonterm=[], rules=[]} $ fmap checkSemantics ( parseGrammar content )
    | action == "1" = return $ show $ fromRight Grammar {nonterminals=[], terminals=[], startNonterm=[], rules=[]} (removeSimple <$> fmap checkSemantics (parseGrammar content))
    | action == "2" = return $ show $ fromRight Grammar {nonterminals=[], terminals=[], startNonterm=[], rules=[]} (toCNF . removeSimple <$> fmap checkSemantics (parseGrammar content))   
    | otherwise = error "Wrong argument" 
