import System.Environment   
import System.IO
import Text.Parsec (char, count, endBy, eof, many1, newline, oneOf, parse, ParseError,
  sepBy, sepBy1, string)
import Text.Parsec.String (Parser)

-- Data types for Grammar representation
type Nonterminal = String
type Nonterminals = [Nonterminal]

type Terminal = String
type Terminals = [Terminal]

-- Nonterminals because of e.g. <Aa>
type Rule = (Nonterminals, [String])
type Rules = [Rule] 

data Grammar = Grammar
    { nonterminals :: Nonterminals,
      terminals :: Terminals,
      startNonterm :: Nonterminal,
      rules :: [Rule]
    } deriving (Show)


parseRules :: Parser String
parseRules = sepBy1 (oneOf ['A'..'Z']) (string "->") <* eof

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


parseGrammar :: String -> Either ParseError Grammar
parseGrammar stringGrammar = parse parseGrammar' "Wrong Grammar Format" stringGrammar

parseGrammar' :: Parser Grammar 
parseGrammar'  = do
    nonterms <- parseNonTerminals 
    terms <- parseTerminals  
    start <- parseStartSymbol
    return Grammar {nonterminals=nonterms, terminals=terms, startNonterm=start, rules=[]}

-- foldl pre rules a vo funkcii && ci je elementom nonterms ++ terms 
checkSemantics :: Grammar -> Grammar
checkSemantics g = 
    if startNonterm g `elem` nonterminals g then g 
    else error "Invalid starting symbol"


main :: IO ()
main = do 
    (action, content) <- parseArgs <$> getArgs
    string_g <- content  
    g <-  performAction action string_g 
    putStrLn g
    return ()


tmp :: [String] -> String
tmp [] = "Empty"
tmp (x:y:z:q) = z
tmp _ = "IDK"

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
    | action == "1" = return $ show $ checkSemantics $ Grammar {nonterminals=[], terminals=[content], startNonterm="S1", rules=[]}  
    | action == "2" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S2", rules=[]}  
    | otherwise = error "Wrong argument" 
