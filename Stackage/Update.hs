module Stackage.Update
    ( stackageUpdate
    , StackageUpdateSettings
    , defaultStackageUpdateSettings
    ) where

import           Control.Exception            (IOException, try)
import           Control.Monad                (when)
import           Data.Version                 (Version, parseVersion)
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               findExecutable,
                                               getAppUserDataDirectory,
                                               removeFile)
import           System.Exit                  (ExitCode (ExitSuccess), exitWith)
import           System.FilePath              ((<.>), (</>))
import           System.IO                    (hPutStrLn, stderr)
import           System.Process               (createProcess, cwd, proc,
                                               readProcess, waitForProcess)
import           Text.ParserCombinators.ReadP (readP_to_S)

-- | Settings for controlling the update process.
--
-- Use 'defaultStackageUpdateSettings' to create a value of this type.
--
-- Since 0.1.0.0
data StackageUpdateSettings = StackageUpdateSettings

-- | Default settings for the update process.
--
-- Since 0.1.0.0
defaultStackageUpdateSettings :: StackageUpdateSettings
defaultStackageUpdateSettings = StackageUpdateSettings

-- | Since internal representation of Version will change in the future.
version19 :: Version
version19 =
    case map fst $ filter (null . snd) $ readP_to_S parseVersion "1.9" of
        x:_ -> x
        [] -> error "Couldn't parse 1.9 as a version"

-- | Perform an update from the Git repository
stackageUpdate :: StackageUpdateSettings -> IO ()
stackageUpdate StackageUpdateSettings = do
    mgit <- findExecutable "git"
    git <-
        case mgit of
            Just git -> return git
            Nothing -> error "Please install git and provide the executable on your PATH"

    -- Check for support of the no-single-branch option
    -- https://github.com/fpco/stackage-update/issues/5
    fullVer <- readProcess git ["--version"] ""
    let hasNSB =
            case reverse $ words fullVer of
                ver:_ ->
                    case map fst $ filter (null . snd) $ readP_to_S parseVersion ver of
                        ver':_ -> ver' >= version19
                        [] -> False
                [] -> False
        cloneArgs =
            "clone" : "https://github.com/commercialhaskell/all-cabal-files.git" : rest
          where
            rest
                | hasNSB =
                    [ "-b", "display" -- avoid checking out a lot of files
                    , "--depth", "1"
                    , "--no-single-branch"
                    ]
                | otherwise =
                    [ "-b", "hackage"
                    ]

    suDir <- getAppUserDataDirectory "stackage-update"
    let acfDir = suDir </> "all-cabal-files"
    repoExists <- doesDirectoryExist acfDir
    if repoExists
        then runIn suDir acfDir "git" ["fetch"]
        else runIn suDir suDir "git" cloneArgs

    cabalDir <- getAppUserDataDirectory "cabal"
    let hackageDir = cabalDir </> "packages" </> "hackage.haskell.org"
    createDirectoryIfMissing True hackageDir

    let tarFile = hackageDir </> "00-index.tar"
        gzFile = tarFile <.> "gz"

    _ <- tryIO $ removeFile tarFile
    runIn suDir acfDir "git" ["archive", "--format=tar", "-o", tarFile, "origin/hackage"]

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

runIn :: FilePath -- ^ su directory
      -> FilePath -- ^ directory
      -> FilePath -- ^ command
      -> [String] -- ^ command line arguments
      -> IO ()
runIn suDir dir cmd args = do
    createDirectoryIfMissing True dir
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
        { cwd = Just dir
        }
    ec <- waitForProcess ph
    when (ec /= ExitSuccess) $ do
        hPutStrLn stderr $ concat
            [ "Exit code "
            , show ec
            , " while running "
            , show (cmd:args)
            , " in "
            , dir
            ]
        hPutStrLn stderr $ concat
            [ "If the problem persists, please delete the following directory "
            , "and try again"
            ]
        hPutStrLn stderr suDir
        exitWith ec
