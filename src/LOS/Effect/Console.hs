{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : LOS.Effect.Console
Description : Console I/O effect
Copyright   : (c) LOS Project, 2026
License     : BSD3

This module provides the Console effect for terminal I/O.

Key insight: Console operations are *described*, not *executed*.
The actual I/O happens only when a handler interprets the description.

This enables:
  - Pure testing (mock the console)
  - Logging (intercept all output)
  - Replay (record and replay sessions)
-}
module LOS.Effect.Console
    ( -- * Effect Type
      Console(..)
      -- * Smart Constructors
    , putLine
    , getLine
    , putStr
      -- * Handlers
    , runConsolePure
    ) where

import Prelude hiding (getLine, putStr)
import LOS.Effect.Core

-- | Console effect: terminal input/output operations.
--
-- This is a GADT representing possible console operations.
-- Each constructor describes an operation and its result type.
data Console a where
    PutLine :: String -> Console ()    -- ^ Print a line
    GetLine :: Console String          -- ^ Read a line
    PutStr  :: String -> Console ()    -- ^ Print without newline

-- | Print a line to the console.
putLine :: Member Console effs => String -> Eff effs ()
putLine s = send (PutLine s)

-- | Read a line from the console.
getLine :: Member Console effs => Eff effs String
getLine = send GetLine

-- | Print a string without newline.
putStr :: Member Console effs => String -> Eff effs ()
putStr s = send (PutStr s)

-- | Handle Console effect purely with predefined input.
--
-- This is a "test" handler that uses a list of strings as input
-- and collects all output.
--
-- @
-- let (output, result) = run $ runConsolePure ["Alice"] $ do
--         putLine "Name?"
--         name <- getLine
--         putLine ("Hello " ++ name)
-- -- output = ["Name?", "Hello Alice"]
-- -- result = ()
-- @
runConsolePure :: [String]  -- ^ Predefined input lines
               -> Eff (Console ': effs) a
               -> Eff effs ([String], a)  -- ^ (Output lines, result)
runConsolePure inputs = go inputs []
  where
    go :: [String] -> [String] -> Eff (Console ': effs) a -> Eff effs ([String], a)
    go _ out (Pure a) = Pure (reverse out, a)
    go inp out (Impure u k) = case decomp u of
        Right (PutLine s) -> go inp (s : out) (k ())
        Right (PutStr s)  -> go inp (s : out) (k ())  -- Simplified
        Right GetLine     -> case inp of
            []     -> error "runConsolePure: no more input"
            (i:is) -> go is out (k i)
        Left u' -> Impure u' (\x -> go inp out (k x))
