import System.Environment   

main :: IO ()
main = do 
    s <- parseArgs <$> getArgs
    putStrLn s
    return ()

parseArgs :: [String] -> String
parseArgs _ = "hello"
