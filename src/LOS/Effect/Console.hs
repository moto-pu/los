{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
    , runConsoleIO
    , runConsolePure
    ) where

import Prelude hiding (getLine, putStr)
import qualified System.IO as IO
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

-- | Handle Console effect by performing real I/O.
--
-- This is the "production" handler that actually does I/O.
runConsoleIO :: Eff (Console ': effs) a -> Eff effs (IO a)
runConsoleIO = \case
    Pure a -> Pure (pure a)
    Impure u k -> case decomp u of
        Right (PutLine s) -> do
            rest <- runConsoleIO (k ())
            Pure $ IO.putStrLn s >> rest
        Right GetLine -> do
            Pure $ do
                s <- IO.getLine
                runEffIO $ runConsoleIO (k s)
        Right (PutStr s) -> do
            rest <- runConsoleIO (k ())
            Pure $ IO.putStr s >> rest
        Left u' -> Impure u' (\x -> runConsoleIO (k x))

-- Helper to run IO from Eff
runEffIO :: Eff '[] (IO a) -> IO a
runEffIO (Pure io) = io
runEffIO _ = error "Impossible"

-- | Handle Console effect purely with predefined input.
--
-- This is a "test" handler that uses a list of strings as input
-- and collects all output.
--
-- @
-- let (output, result) = runConsolePure ["Alice"] $ do
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

-- | Decompose a union: is it this effect or another?
decomp :: Union (eff ': effs) a -> Either (Union effs a) (eff a)
decomp (Here x)  = Right x
decomp (There u) = Left u
