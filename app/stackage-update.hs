import Stackage.Update
import System.Environment (getArgs)
import Data.List ((++), concat, intersperse)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn $ "Usage: stackage-update \n" ++
                      "Run this command with no arguments to update your package index."
        ["--summary"] -> putStrLn "Update your package index incrementally (requires git)"
        [] -> stackageUpdate defaultStackageUpdateSettings
        xs -> putStrLn $ "stackage-update: unrecognized argument \'" ++
              (concat $ intersperse " " xs) ++ "\'\nTry \'stackage-update --help\' for more information"

