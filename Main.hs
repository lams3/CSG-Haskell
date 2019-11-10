import CSG
import CSG.Parser
import System.IO

main = do
    handle <- openFile "Test.csg" ReadMode
    contents <- hGetContents handle
    case parse contents of
        Nothing -> putStrLn "Couldn't parse Test.csg"
        Just s -> putStrLn (show s)