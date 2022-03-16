import System.Environment   

main :: IO ()
main = do 
    getArgs >>= parseArgs
    return ()


parseArgs ("-i":xs) = fmap putStrLn xs 
parseArgs a = sequence (map putStrLn a)
