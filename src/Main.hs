import System.Environment   
import Text.Parsec (char, endBy, eof, many1, newline, oneOf, parse, ParseError, sepBy1, string)
import Text.Parsec.String (Parser)
import Data.List (nub)

-- Data types for Grammar representation
type Nonterminal = String
type Nonterminals = [Nonterminal]

type Terminal = String
type Terminals = [Terminal]

-- Nonterminal is string because of e.g. <Aa>
type Rule = (Nonterminal, [String])
type Rules = [Rule] 

type RuleString = [String]

type NaSet = (Nonterminal, Nonterminals)
type NaSets = [NaSet]


data Grammar = Grammar
    { nonterminals :: Nonterminals,
      terminals :: Terminals,
      startNonterm :: Nonterminal,
      rules :: [Rule]
    } deriving (Show)

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
    where newRules = createRules (createNa (nonterminals g) (rules g)) (rules g)
 

-- TODO GET SIMPLE RULES BEFORE AND REMOVE SIMPLE RULES BEFORE createRules FUNCTION
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

-- get list of simple rules A->B
getSimpleRules :: Rules -> Nonterminals -> Rules
getSimpleRules rs nonts = filter ( \r -> any (`elem` nonts) (snd r) && length (snd r) == 1) rs

createRules :: NaSets -> Rules -> Rules
createRules [] _ = []
createRules (x:xs) rs = createRules' x rs : createRules xs rs 

createRules' :: NaSet -> Rules -> Rules
createRules' nas rs = filter (\r -> fst r /= "") (map (\r -> if fst r `elem` snd nas then (fst nas, snd r) else ("",[])) rs)

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
    | action == "-i" = return $ show $ fmap checkSemantics ( parseGrammar content )
    | action == "1" = return $ show $ fmap checkSemantics ( parseGrammar content )  
    | action == "2" = return $ show $ fmap checkSemantics ( parseGrammar content )   
    | otherwise = error "Wrong argument" 
