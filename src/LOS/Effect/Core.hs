{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : LOS.Effect.Core
Description : Core types for the effect system
Copyright   : (c) LOS Project, 2026
License     : BSD3

This module provides the core types for LOS's effect system,
based on the Freer Monad approach (Extensible Effects).

The key insight is that effects are *data*, not control flow.
An effectful computation is a *description* of what effects to perform,
not the effects themselves. Handlers interpret these descriptions.

@
-- Example: A program that uses Console effect
program :: Member Console effs => Eff effs ()
program = do
    putLine "What is your name?"
    name <- getLine
    putLine ("Hello, " ++ name)

-- The program doesn't *do* IO; it *describes* IO.
-- A handler decides what to do with the description.
@
-}
module LOS.Effect.Core
    ( -- * Core Types
      Eff(..)
    , Member
    , run
    , send
      -- * Effect Composition
    , type (:<)
    , type (:<<)
    ) where

import Data.Kind (Type)

-- | Open union of effects.
-- This represents "one of the effects in the list".
data Union (effs :: [Type -> Type]) a where
    Here  :: eff a -> Union (eff ': effs) a
    There :: Union effs a -> Union (eff ': effs) a

-- | The Eff monad: a free monad over an open union of effects.
--
-- 'Eff effs a' represents a computation that:
--   - May perform effects from the list 'effs'
--   - Eventually produces a value of type 'a'
--
-- This is the central type of the effect system.
data Eff (effs :: [Type -> Type]) a where
    Pure   :: a -> Eff effs a
    Impure :: Union effs x -> (x -> Eff effs a) -> Eff effs a

instance Functor (Eff effs) where
    fmap f (Pure a)       = Pure (f a)
    fmap f (Impure u k)   = Impure u (fmap f . k)

instance Applicative (Eff effs) where
    pure = Pure
    Pure f     <*> a = fmap f a
    Impure u k <*> a = Impure u (\x -> k x <*> a)

instance Monad (Eff effs) where
    Pure a     >>= f = f a
    Impure u k >>= f = Impure u (\x -> k x >>= f)

-- | Membership constraint: effect 'eff' is in the list 'effs'.
class Member (eff :: Type -> Type) (effs :: [Type -> Type]) where
    inj :: eff a -> Union effs a
    prj :: Union effs a -> Maybe (eff a)

instance {-# OVERLAPPING #-} Member eff (eff ': effs) where
    inj = Here
    prj (Here x)  = Just x
    prj (There _) = Nothing

instance Member eff effs => Member eff (eff' ': effs) where
    inj = There . inj
    prj (Here _)  = Nothing
    prj (There u) = prj u

-- | Type-level membership (infix alias)
type eff :< effs = Member eff effs

-- Note: For full :<< constraint, use GHC.Exts.Constraint
-- Simplified for PoC

-- | Lift an effect into the Eff monad.
send :: Member eff effs => eff a -> Eff effs a
send eff = Impure (inj eff) Pure

-- | Run a pure computation (no effects remaining).
run :: Eff '[] a -> a
run (Pure a) = a
run (Impure _ _) = error "Impossible: no effects to handle"
