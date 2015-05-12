import Stackage.Update
import System.Environment (getArgs)
import Data.List ((++), concat, intersperse)
import Paths_stackage_update (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn $ unlines
            [ "Usage: stackage-update [--[no-]verify] [--[no-]hashes]"
            , "Run this command with no arguments to update your package index."
            , ""
            , "    --[no-]verify : Verify GPG signature on the repo"
            , "    --[no-]hashes : Download from the all-cabal-hashes repo"
            ]
        ["--summary"] -> putStrLn "Update your package index incrementally (requires git)"
        ["--version"] -> putStrLn $ "stackage-update version " ++ showVersion version
        _ -> stackageUpdate $ foldr addArg defaultStackageUpdateSettings args

addArg :: String -> StackageUpdateSettings -> StackageUpdateSettings
addArg "--verify" = setVerify True
addArg "--no-verify" = setVerify False
addArg "--hashes" = setRemote allCabalHashes . setDirectoryName "all-cabal-hashes"
addArg "--no-hashes" = setRemote allCabalFiles . setDirectoryName "all-cabal-files"
addArg s = error $ concat
    [ "Did not understand argument "
    , show s
    , ", try 'stackage-update --help' for more information"
    ]
