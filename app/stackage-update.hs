import Stackage.Update
import System.Environment (getArgs)
import Data.List ((++), concat, intersperse)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn $ unlines
            [ "Usage: stackage-update [--verify] [--hashes]"
            , "Run this command with no arguments to update your package index."
            , ""
            , "    --verify : Verify GPG signature on the repo"
            , "    --hashes : Download from the all-cabal-hashes repo"
            ]
        ["--summary"] -> putStrLn "Update your package index incrementally (requires git)"
        _ -> stackageUpdate $ foldr addArg defaultStackageUpdateSettings args

addArg :: String -> StackageUpdateSettings -> StackageUpdateSettings
addArg "--verify" = setVerify True
addArg "--hashes" = setRemote allCabalHashes . setDirectoryName "all-cabal-hashes"
addArg s = error $ concat
    [ "Did not understand argument "
    , show s
    , ", try 'stackage-update --help' for more information"
    ]
