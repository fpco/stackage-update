import Stackage.Update
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn "Run this command with no arguments to update your package index"
        ["--summary"] -> putStrLn "Update your package index incrementally (requires git)"
        _ -> stackageUpdate defaultStackageUpdateSettings
