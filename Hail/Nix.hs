{-# LANGUAGE LambdaCase #-}
module Hail.Nix where

import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

import System.Process (spawnProcess, waitForProcess, callProcess)
import System.FilePath ((</>))

-- | Run the activation script for a profile.
--
-- Errors are logged to stderr.
activate
  :: FilePath -- ^ The path to the profile
  -> ActivateErrorMode
  -> IO ()
activate profilePath error_mode = do
    e_pid <- tryIOError $ spawnProcess activateCommand []
    case e_pid of
      Left err ->
        logger $ "spawning activation command failed: " ++ show err
      Right pid -> do
        code <- waitForProcess pid
        case code of
          ExitSuccess -> return ()
          ExitFailure i -> logger $
            "activation command exited with non-zero code " ++ show i
  where
    logger = case error_mode of
      ActivateIgnoreErrors -> const $ return ()
      ActivateReportErrors -> hPutStrLn stderr
    activateCommand = profilePath </> "bin/activate"

-- | Settings for error handling for 'activate'.
data ActivateErrorMode
  = ActivateIgnoreErrors -- ^ Ignore errors.
  | ActivateReportErrors -- ^ Log errors to stderr.

-- | Update a profile to point to a given store path.
--
-- The store path is realised if necessary.
--
-- Errors are logged to stderr.
switchProfile
  :: FilePath -- ^ The profile.
  -> FilePath -- ^ The store path.
  -> IO Bool  -- ^ Whether the update succeeded.
switchProfile profilePath outPath = tryIOError switch >>= \case
    Left err -> do
      hPutStrLn stderr $  "Switching " ++ profilePath ++ " to "
                       ++ outPath ++ " failed: " ++ (show err)
      return False
    Right () -> return True
  where
    -- TODO Hard-code path to nix-env?
    switch = callProcess "nix-env" [ "--profile", profilePath
                                   , "--set", outPath
                                   ]
