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

type Rule = (Nonterminal, Terminals)
type Rules = [Rule] 

data Grammar = Grammar
    { nonterminals :: Nonterminals,
      terminals :: Terminals,
      startNonterm :: Nonterminal,
      rules :: [Rule]
    } deriving (Show)

parseStartSymbol :: Parser String 
parseStartSymbol =  fmap (\x -> [x]) (oneOf ['A'..'Z']) <* newline

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









parseGrammar :: String -> Either ParseError Grammar
parseGrammar stringGrammar = parse parseGrammar' "Wrong Grammar Format" stringGrammar

parseGrammar' :: Parser Grammar 
parseGrammar'  = do
    nonterms <- parseNonTerminals 
    terms <- parseTerminals  
    start <- parseStartSymbol
    return Grammar {nonterminals=nonterms, terminals=terms, startNonterm=start, rules=[]}



main :: IO ()
main = do 
    (action, content) <- parseArgs <$> getArgs
    file_path <- content  
    a <-  performAction action file_path
    putStrLn a

    --putStrLn $ head $ lines file_path
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
    | action == "-i" = return $ show $ parseGrammar content 
    | action == "1" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S1", rules=[]}  
    | action == "2" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S2", rules=[]}  
    | otherwise = error "Wrong argument" 
