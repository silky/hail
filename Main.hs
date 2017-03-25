{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (when)
import Control.Applicative ((<*>))
import Control.Concurrent (threadDelay)
import Data.Semigroup ((<>))
import System.IO (hPutStrLn, stderr)

import System.FilePath ((</>), takeDirectory)

import System.Directory (canonicalizePath, createDirectoryIfMissing)

import Options.Applicative

import Network.Wreq (Auth)

import Hail.Netrc
import Hail.Nix
import Hail.Hydra

-- | Command line options
data Opts = Opts
  { profile :: String           -- ^ The profile to install the
                                -- service into.
  , jobURI :: String            -- ^ The job to poll.
  , netrcFile :: Maybe FilePath -- ^ The netrc file for hydra access.
  , pollPeriod :: Maybe Int     -- ^ The period to poll the job, in
                                -- minutes, or 'Nothing' for a oneshot.
  }

-- | Parser for the poll period command line flag
pollPeriodParser :: Parser (Maybe Int)
pollPeriodParser =  Just
                <$> option auto
                  ( long "poll-period"
                 <> metavar "PERIOD"
                 <> help "The period with which to poll, in minutes"
                 <> value 5
                 <> showDefault)

-- | Parser for the oneshot command line flag
oneshotParser :: Parser (Maybe Int)
oneshotParser = flag' Nothing
                 ( long "oneshot"
                <> help "Just update once, rather than in a loop"
                 )

-- | Parser for command line options
optsParser :: Parser Opts
optsParser =  Opts
          <$> strOption
              ( long "profile"
             <> metavar "PROFILE"
             <> help "The nix profile to install the service into")
          <*> strOption
              ( long "job-uri"
             <> metavar "HYDRA_JOB_URI"
             <> help "The hydra job to poll")
          <*> (optional $ strOption
              ( long "netrc-file"
             <> metavar "NETRC_FILE"
             <> help "The netrc file for hydra HTTP access"))
          <*> (oneshotParser <|> pollPeriodParser)

-- | Full command line parser with usage string.
optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper)
  ( fullDesc
 <> progDesc "Pull builds from HYDRA_JOB_URI into PROFILE"
 <> header "hail - Pull-based continuous deployment from hydra")

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  let profilePath = "/nix/var/nix/profiles" </> (profile opts)
      uri = jobURI opts
      cont m_creds = case pollPeriod opts of
        Nothing -> \_ -> return ()
        Just period -> \delay -> do
          case delay of
            Delay -> threadDelay $ minutesToMicroseconds period
            NoDelay -> return ()
          pollLoop profilePath uri m_creds (cont m_creds)
  m_creds <- loadCredsFromNetrc (netrcFile opts) uri
  createDirectoryIfMissing True $ takeDirectory profilePath
  -- Try to activate on initial startup, but ignore failures.
  activate profilePath ActivateIgnoreErrors
  cont m_creds NoDelay

-- | Convert minutes to microseconds
minutesToMicroseconds :: Int -> Int
minutesToMicroseconds = (*) $ 60 * 1000000

-- | Whether the looping continuation should delay before
-- the next iteration of the loop.
data ShouldDelay = Delay | NoDelay

-- | Poll hydra for new builds, with an explicit continuation
pollLoop :: FilePath               -- ^ The profile path
         -> String                 -- ^ The job URI
         -> Maybe Auth             -- ^ The creds for talking to hydra
         -> (ShouldDelay -> IO ()) -- ^ The continuation
         -> IO ()
pollLoop profilePath uri m_creds cont =
  getLatest uri m_creds >>= \case
    Left msg -> do
      hPutStrLn stderr msg
      cont Delay
    Right outPath -> do
      prevOutPath <- canonicalizePath profilePath
      case prevOutPath /= outPath of
        True -> do
          switchSucceeded <- switchProfile profilePath outPath
          when switchSucceeded $
            activate profilePath ActivateReportErrors
          cont NoDelay
        False -> cont Delay
