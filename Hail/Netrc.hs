{-# LANGUAGE LambdaCase #-}
module Hail.Netrc (loadCredsFromNetrc) where

import Control.Monad (join)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import System.IO.Error (tryIOError)

import Network.NetRc
import Network.URI
import Network.Wreq (basicAuth, Auth)
import Text.Parsec.Error (ParseError)
import Data.ByteString.Char8 as BS (null, pack)
import Text.Parsec.ByteString (parseFromFile)
import Control.Lens.Operators

-- | Load credentials for a given URI from the netrc file.
--
-- Users of this function cannot distinguish a missing/malformed
-- netrc file from one without an entry for their URI.
loadCredsFromNetrc
  :: Maybe FilePath -- ^ The path to the netrc file.
                    --
                    -- Defaults to "/etc/netrc".
  -> String         -- ^ The URI to look up.
  -> IO (Maybe Auth)
loadCredsFromNetrc m_file uri =
    (tryIOError $ loadCredsFromFile file uri) <&> \case
      Right (Right m_auth) -> m_auth
      _ -> Nothing
  where
    file = fromMaybe "/etc/netrc" m_file

-- | Load credentials for a given URI from a given file.
loadCredsFromFile
  :: FilePath
  -> String -- ^ The URI to look up.
  -> IO (Either ParseError (Maybe Auth))
loadCredsFromFile file uri = lookupAuthInNetrc uri <$$> netrc
  where
    netrc = parseFromFile netRcParsec file
    -- This belongs somewhere else...
    (<$$>) = fmap . fmap

-- | Find credentials for a given URI in a 'NetRc'.
lookupAuthInNetrc
  :: String -- ^ The URI to look up.
  -> NetRc
  -> Maybe Auth
lookupAuthInNetrc uri rc =
    basicAuth <$> nrhLogin <*> nrhPassword <$> m_rcHost
  where
    -- Extract the authority from the URI
    m_authority = join $ uriAuthority <$> parseURI uri
    -- Extract the reg name from the authority
    m_regName = uriRegName <$> m_authority
    -- Predicate determining if a 'NetRcHost' matches a regName
    nameMatch regName (NetRcHost nm _ _ _ _) =
      -- A 'null' name means the default entry
      nm == regName || BS.null nm
    -- Find a matching rcHost based on the regName
    m_rcHost = case m_regName of
      Nothing -> Nothing
      Just regName -> find (nameMatch $ pack regName) $ nrHosts rc
