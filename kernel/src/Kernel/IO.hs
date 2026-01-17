{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}

{-|
Module      : Kernel.IO
Description : Low-level I/O port access
Copyright   : (c) LOS Project, 2026
License     : BSD3

Provides direct access to x86 I/O ports using inline assembly.
-}
module Kernel.IO
    ( inb
    , outb
    , ioWait
    ) where

import Data.Word
import GHC.Exts
import GHC.Word

-- | Read a byte from an I/O port
foreign import ccall unsafe "inb"
    c_inb :: Word16 -> IO Word8

-- | Write a byte to an I/O port
foreign import ccall unsafe "outb"
    c_outb :: Word16 -> Word8 -> IO ()

inb :: Word16 -> IO Word8
inb = c_inb

outb :: Word16 -> Word8 -> IO ()
outb = c_outb

-- | Wait for I/O to complete (for slow devices)
ioWait :: IO ()
ioWait = outb 0x80 0
