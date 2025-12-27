{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- SPDX-License-Identifier: BSD-3-Clause

-- | Spindle.Registry - A minimal config registry for Nickel configurations
--
-- This module provides a simple file-based registry for storing, listing,
-- and retrieving validated Nickel configuration entries.
module Spindle.Registry
  ( -- * Types
    RegistryEntry(..)
  , Registry(..)
  , RegistryError(..)
    -- * Registry Operations
  , emptyRegistry
  , register
  , unregister
  , lookupEntry
  , listEntries
    -- * Persistence
  , loadRegistry
  , saveRegistry
  , defaultRegistryPath
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | A single registry entry containing a validated config
data RegistryEntry = RegistryEntry
  { entryName        :: Text       -- ^ Unique identifier
  , entryVersion     :: Text       -- ^ Semantic version
  , entryDescription :: Maybe Text -- ^ Optional description
  , entrySourcePath  :: FilePath   -- ^ Original .ncl file path
  , entryRegistered  :: UTCTime    -- ^ When it was registered
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | The registry: a map from name to entry
newtype Registry = Registry
  { registryEntries :: Map Text RegistryEntry
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Errors that can occur during registry operations
data RegistryError
  = EntryNotFound Text
  | EntryAlreadyExists Text
  | RegistryIOError String
  | RegistryParseError String
  deriving (Show, Eq)

-- | Create an empty registry
emptyRegistry :: Registry
emptyRegistry = Registry Map.empty

-- | Register a new config entry
register :: RegistryEntry -> Registry -> Either RegistryError Registry
register entry (Registry entries) =
  let name = entryName entry
  in if Map.member name entries
     then Left (EntryAlreadyExists name)
     else Right $ Registry (Map.insert name entry entries)

-- | Remove an entry from the registry
unregister :: Text -> Registry -> Either RegistryError Registry
unregister name (Registry entries) =
  if Map.member name entries
  then Right $ Registry (Map.delete name entries)
  else Left (EntryNotFound name)

-- | Look up an entry by name
lookupEntry :: Text -> Registry -> Either RegistryError RegistryEntry
lookupEntry name (Registry entries) =
  case Map.lookup name entries of
    Just entry -> Right entry
    Nothing    -> Left (EntryNotFound name)

-- | List all entries in the registry
listEntries :: Registry -> [RegistryEntry]
listEntries (Registry entries) = Map.elems entries

-- | Default path for the registry file
defaultRegistryPath :: FilePath
defaultRegistryPath = ".spindle/registry.json"

-- | Load registry from disk
loadRegistry :: FilePath -> IO (Either RegistryError Registry)
loadRegistry path = do
  exists <- doesFileExist path
  if not exists
    then return (Right emptyRegistry)
    else do
      content <- B.readFile path
      case eitherDecodeStrict' content of
        Left err  -> return $ Left (RegistryParseError err)
        Right reg -> return $ Right reg

-- | Save registry to disk
saveRegistry :: FilePath -> Registry -> IO (Either RegistryError ())
saveRegistry path registry = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode registry)
  return (Right ())
