{-# LANGUAGE OverloadedStrings #-}
module Hail.Hydra where

import Control.Exception (handle)

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Data.Aeson.Lens
import Control.Lens.Operators
import Data.Text (unpack)

-- | Get the out path of the latest build of a given hydra job.
getLatest
  :: String     -- ^ The URI to the hydra job.
  -> Maybe Auth -- ^ The credentials to talk to hydra.
  -> IO (Either String FilePath)
getLatest uri m_creds =
    handle (return . Left . (show :: HttpException -> String)) $ do
      r <- getWith opts latestUri
      let m_outPath = r ^? responseBody . key "buildoutputs"
                                        . key "out"
                                        . key "path" . _String
      return $ case m_outPath of
        Nothing -> Left noOutErr
        Just outPath -> Right $ unpack outPath
  where
    opts = defaults & header "Accept" .~ ["application/json"]
                    & auth .~ m_creds
    noOutErr =
      "Latest build of job " ++ uri
                             ++ " has no output named 'out'."
    latestUri = uri ++ "/latest"
