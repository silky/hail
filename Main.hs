{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (when, forever)
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
  { profile :: String -- ^ The profile to install the service into.
  , jobURI :: String -- ^ The job to poll.
  , netrcFile :: Maybe FilePath -- ^ The netrc file for hydra access.
  , pollPeriod :: Int -- ^ The period to poll the job, in minutes.
  , oneshot :: Bool -- ^ Whether to update once or in a loop
  }

-- | Parser for command line options
optsParser :: Parser Opts
optsParser = Opts
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
          <*> option auto
              ( long "poll-period"
             <> metavar "PERIOD"
             <> help "The period with which to poll, in minutes"
             <> value 5
             <> showDefault)
          <*> switch
              ( long "oneshot"
             <> help "Just update once, rather than in a loop"
              )

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
      microPeriod = minutesToMicroseconds $ pollPeriod opts
      go = case oneshot opts of
        True -> checkOnce
        False -> pollLoop
  m_creds <- loadCredsFromNetrc (netrcFile opts) uri
  createDirectoryIfMissing True $ takeDirectory profilePath
  -- Try to activate on initial startup, but ignore failures.
  activate profilePath ActivateIgnoreErrors
  go profilePath uri m_creds microPeriod

-- | Convert minutes to microseconds
minutesToMicroseconds :: Int -> Int
minutesToMicroseconds = (*) $ 60 * 1000000

-- | Check hydra for new builds.
checkOnce :: FilePath -> String -> Maybe Auth -> Int -> IO ()
checkOnce profilePath uri m_creds period =
  getLatest uri m_creds >>= \case
    Left msg -> do
      hPutStrLn stderr msg
      threadDelay period
    Right outPath -> do
      prevOutPath <- canonicalizePath profilePath
      case prevOutPath /= outPath of
        True -> do
          switchSucceeded <- switchProfile profilePath outPath
          when switchSucceeded $
            activate profilePath ActivateReportErrors
        False -> threadDelay period

-- | Poll hydra for new builds, forever
pollLoop :: FilePath -> String -> Maybe Auth -> Int -> IO ()
pollLoop profilePath uri m_creds = forever . checkOnce profilePath uri m_creds
