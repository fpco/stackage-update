module Stackage.Update
    ( stackageUpdate
    , StackageUpdateSettings
    , defaultStackageUpdateSettings
    , setVerify
    , setRemote
    , setDirectoryName
    , allCabalFiles
    , allCabalHashes
    ) where

import           Control.Exception            (IOException, try)
import           Control.Monad                (when, unless)
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
    { verify :: Bool
    , remote :: String
    , name :: FilePath
    }

-- | Should we verify the signature on the Git tag.
--
-- Default: False
--
-- Since 0.1.1.0
setVerify :: Bool -> StackageUpdateSettings -> StackageUpdateSettings
setVerify x s = s { verify = x }

-- | Remote repository to use
--
-- Default: 'allCabalFiles'
--
-- Since 0.1.1.0
setRemote :: String -> StackageUpdateSettings -> StackageUpdateSettings
setRemote x s = s { remote = x }

-- | Local directory name to clone into
--
-- Default: \"all-cabal-files\"
--
-- Since 0.1.1.0
setDirectoryName :: FilePath -> StackageUpdateSettings -> StackageUpdateSettings
setDirectoryName x s = s { name = x }

-- | Default settings for the update process.
--
-- Since 0.1.0.0
defaultStackageUpdateSettings :: StackageUpdateSettings
defaultStackageUpdateSettings = StackageUpdateSettings
    { verify = False
    , remote = allCabalFiles
    , name = "all-cabal-files"
    }

-- | URL for the all-cabal-files repo
--
-- Since 0.1.1.0
allCabalFiles :: String
allCabalFiles = "https://github.com/commercialhaskell/all-cabal-files.git"

-- | URL for the all-cabal-hashes repo
--
-- Since 0.1.1.0
allCabalHashes :: String
allCabalHashes = "https://github.com/commercialhaskell/all-cabal-hashes.git"

-- | Perform an update from the Git repository
stackageUpdate :: StackageUpdateSettings -> IO ()
stackageUpdate set = do
    mgit <- findExecutable "git"
    git <-
        case mgit of
            Just git -> return git
            Nothing -> error "Please install git and provide the executable on your PATH"

    let cloneArgs =
            [ "clone", remote set, name set
            , "--depth", "1"
            , "-b", "display"
            ]

    sDir <- getAppUserDataDirectory "stackage"
    let suDir = sDir </> "update"
        acfDir = suDir </> name set
    repoExists <- doesDirectoryExist acfDir

    unless repoExists $ do
        putStrLn "Cloning repository for first time"
        runIn suDir suDir "git" cloneArgs Nothing

    runIn suDir acfDir "git"
        [ "fetch"
        , "--tags"
        , "--depth=1"
        ] Nothing

    cabalDir <- getAppUserDataDirectory "cabal"
    let hackageDir = cabalDir </> "packages" </> "hackage.haskell.org"
    createDirectoryIfMissing True hackageDir

    let tarFile = hackageDir </> "00-index.tar"
        gzFile = tarFile <.> "gz"

    _ <- tryIO $ removeFile tarFile
    when (verify set) $ do
        runIn suDir acfDir "git" ["tag", "-v", "current-hackage"] $ Just $ unlines
            [ "Signature verification failed. Please ensure you've set up your"
            , "GPG keychain to accept the D6CF60FD signing key."
            , "For more information, see:"
            , "https://github.com/fpco/stackage-update#readme"
            ]
    runIn suDir acfDir "git" ["archive", "--format=tar", "-o", tarFile, "current-hackage"] Nothing

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

runIn :: FilePath -- ^ su directory
      -> FilePath -- ^ directory
      -> FilePath -- ^ command
      -> [String] -- ^ command line arguments
      -> Maybe String -- ^ error message
      -> IO ()
runIn suDir dir cmd args errMsg = do
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
        hPutStrLn stderr $ maybe defErrMsg id errMsg
        exitWith ec

defErrMsg :: String
defErrMsg = concat
    [ "If the problem persists, please delete the following directory "
    , "and try again"
    ]
