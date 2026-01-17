{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-|
Module      : Kernel.Boot
Description : Kernel boot sequence
Copyright   : (c) LOS Project, 2026
License     : BSD3

This module handles the kernel boot sequence:
1. Initialize hardware (VGA, keyboard)
2. Display boot messages
3. Perform login
4. Start shell
-}
module Kernel.Boot
    ( kernelMain
    ) where

import Kernel.VGA
import Kernel.Keyboard
import Kernel.Shell
import Kernel.Login

-- | Kernel entry point, called from assembly boot code
kernelMain :: IO ()
kernelMain = do
    -- Initialize VGA
    vgaInit
    vgaClear

    -- Boot messages
    vgaSetColor VGALightGreen VGABlack
    vgaPuts "LOS - Lambda Operating System\n"
    vgaSetColor VGALightGrey VGABlack
    vgaPuts "Version 0.1.0 (PoC)\n"
    vgaPuts "\n"

    -- Initialize keyboard
    keyboardInit
    vgaPuts "[OK] Keyboard initialized\n"

    vgaPuts "[OK] Boot sequence complete\n"
    vgaPuts "\n"

    -- Login loop
    loginLoop

    -- Start shell (after successful login)
    shellMain

    -- Should never reach here
    vgaPuts "\nSystem halted."
    halt

-- | Halt the CPU
halt :: IO ()
halt = do
    -- Infinite loop (will be replaced with proper halt instruction)
    halt
