{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : LOS.Effect.FileSystem
Description : File system effect
Copyright   : (c) LOS Project, 2026
License     : BSD3

This module provides the FileSystem effect for file operations.

Like Console, file operations are described, not executed.
This enables:
  - Pure testing with in-memory filesystems
  - Sandboxing (restrict file access)
  - Virtualization (overlay filesystems)
-}
module LOS.Effect.FileSystem
    ( -- * Effect Type
      FileSystem(..)
      -- * Smart Constructors
    , readFile
    , writeFile
    , appendFile
    , doesFileExist
    , listDirectory
      -- * Handlers
    , runFileSystemPure
    , VirtualFS
    ) where

import Prelude hiding (readFile, writeFile, appendFile, FilePath)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import LOS.Effect.Core

type FilePath = String

-- | FileSystem effect: file operations.
data FileSystem a where
    ReadFile      :: FilePath -> FileSystem String
    WriteFile     :: FilePath -> String -> FileSystem ()
    AppendFile    :: FilePath -> String -> FileSystem ()
    DoesFileExist :: FilePath -> FileSystem Bool
    ListDirectory :: FilePath -> FileSystem [FilePath]

-- | Read entire file contents.
readFile :: Member FileSystem effs => FilePath -> Eff effs String
readFile path = send (ReadFile path)

-- | Write string to file (overwrite).
writeFile :: Member FileSystem effs => FilePath -> String -> Eff effs ()
writeFile path content = send (WriteFile path content)

-- | Append string to file.
appendFile :: Member FileSystem effs => FilePath -> String -> Eff effs ()
appendFile path content = send (AppendFile path content)

-- | Check if file exists.
doesFileExist :: Member FileSystem effs => FilePath -> Eff effs Bool
doesFileExist path = send (DoesFileExist path)

-- | List directory contents.
listDirectory :: Member FileSystem effs => FilePath -> Eff effs [FilePath]
listDirectory path = send (ListDirectory path)

-- | In-memory filesystem for testing.
type VirtualFS = Map FilePath String

-- | Handle FileSystem effect with in-memory filesystem.
--
-- @
-- let initialFS = Map.fromList [("hello.txt", "Hello!")]
--     (finalFS, content) = run $ runFileSystemPure initialFS $ do
--         readFile "hello.txt"
-- -- content = "Hello!"
-- @
runFileSystemPure :: VirtualFS
                  -> Eff (FileSystem ': effs) a
                  -> Eff effs (VirtualFS, a)
runFileSystemPure fs = go fs
  where
    go :: VirtualFS -> Eff (FileSystem ': effs) a -> Eff effs (VirtualFS, a)
    go vfs (Pure a) = Pure (vfs, a)
    go vfs (Impure u k) = case decomp u of
        Right (ReadFile path) ->
            case Map.lookup path vfs of
                Just content -> go vfs (k content)
                Nothing      -> error $ "File not found: " ++ path
        Right (WriteFile path content) ->
            go (Map.insert path content vfs) (k ())
        Right (AppendFile path content) ->
            let current = Map.findWithDefault "" path vfs
            in go (Map.insert path (current ++ content) vfs) (k ())
        Right (DoesFileExist path) ->
            go vfs (k (Map.member path vfs))
        Right (ListDirectory _) ->
            go vfs (k (Map.keys vfs))  -- Simplified: return all files
        Left u' -> Impure u' (\x -> go vfs (k x))
