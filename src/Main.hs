import System.Environment   

main :: IO ()
main = do 
    (action, file_path) <- parseArgs <$> getArgs
    putStrLn action
    putStrLn file_path
    return ()

parseArgs :: [String] -> (String, String) 
parseArgs [] = error "No arguments" 
parseArgs [x] 
    | x `elem` ["-i", "1", "2"] = (x, "STDIN")
    | otherwise = error "Wrong first argument"
parseArgs (x:y:_) 
    | x `elem` ["-i", "1", "2"] = (x, y)
    | otherwise = error "Wrong first argument"
