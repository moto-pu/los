{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : LOS.Effect.State
Description : State effect
Copyright   : (c) LOS Project, 2026
License     : BSD3

This module provides the State effect for mutable state.

In traditional FP, State is handled via the State monad.
With effects, State becomes composable with other effects.
-}
module LOS.Effect.State
    ( -- * Effect Type
      State(..)
      -- * Smart Constructors
    , get
    , put
    , modify
      -- * Handlers
    , runState
    , evalState
    , execState
    ) where

import LOS.Effect.Core

-- | State effect: mutable state operations.
data State s a where
    Get :: State s s
    Put :: s -> State s ()

-- | Get the current state.
get :: Member (State s) effs => Eff effs s
get = send Get

-- | Set the state.
put :: Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

-- | Modify the state with a function.
modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = do
    s <- get
    put (f s)

-- | Handle State effect, returning both result and final state.
runState :: s -> Eff (State s ': effs) a -> Eff effs (s, a)
runState s = go s
  where
    go :: s -> Eff (State s ': effs) a -> Eff effs (s, a)
    go st (Pure a) = Pure (st, a)
    go st (Impure u k) = case decomp u of
        Right Get      -> go st (k st)
        Right (Put s') -> go s' (k ())
        Left u'        -> Impure u' (\x -> go st (k x))

-- | Handle State effect, returning only the result.
evalState :: s -> Eff (State s ': effs) a -> Eff effs a
evalState s eff = snd <$> runState s eff

-- | Handle State effect, returning only the final state.
execState :: s -> Eff (State s ': effs) a -> Eff effs s
execState s eff = fst <$> runState s eff

-- | Decompose a union.
decomp :: Union (eff ': effs) a -> Either (Union effs a) (eff a)
decomp (Here x)  = Right x
decomp (There u) = Left u
