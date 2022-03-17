import System.Environment   
import System.IO




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






main :: IO ()
main = do 
    (action, content) <- parseArgs <$> getArgs
    file_path <- content  
    a <-  performAction action file_path
    putStrLn a
    --putStrLn $ head $ lines file_path
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
    | action == "-i" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S", rules=[]}  
    | action == "1" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S1", rules=[]}  
    | action == "2" = return $ show $ Grammar {nonterminals=[], terminals=[content], startNonterm="S2", rules=[]}  
    | otherwise = error "Wrong argument" 


dumb :: String -> Grammar
dumb _ = Grammar {nonterminals=[], terminals=[], startNonterm="", rules=[]}
