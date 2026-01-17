{-|
Module      : Kernel.Login
Description : Simple login system
Copyright   : (c) LOS Project, 2026
License     : BSD3

A minimal login system for the kernel.
For PoC purposes, accepts a hardcoded username/password.
-}
module Kernel.Login
    ( loginLoop
    ) where

import Kernel.VGA
import Kernel.Keyboard

-- | Hardcoded credentials (PoC only!)
validUsername, validPassword :: String
validUsername = "root"
validPassword = "lambda"

-- | Read a line from keyboard, echoing to screen
readLine :: IO String
readLine = go []
  where
    go acc = do
        c <- keyboardGetChar
        case c of
            '\n' -> do
                vgaPutChar '\n'
                return (reverse acc)
            '\b' -> case acc of
                [] -> go acc
                (_:rest) -> do
                    -- Erase character on screen
                    vgaPuts "\b \b"
                    go rest
            _ -> do
                vgaPutChar c
                go (c : acc)

-- | Read a line without echo (for password)
readLineNoEcho :: IO String
readLineNoEcho = go []
  where
    go acc = do
        c <- keyboardGetChar
        case c of
            '\n' -> do
                vgaPutChar '\n'
                return (reverse acc)
            '\b' -> case acc of
                [] -> go acc
                (_:rest) -> go rest
            _ -> do
                vgaPutChar '*'  -- Show asterisk instead
                go (c : acc)

-- | Main login loop
loginLoop :: IO ()
loginLoop = do
    vgaPuts "LOS Login\n"
    vgaPuts "=========\n\n"
    attempt
  where
    attempt = do
        vgaPuts "Username: "
        username <- readLine

        vgaPuts "Password: "
        password <- readLineNoEcho

        if username == validUsername && password == validPassword
            then do
                vgaPuts "\n"
                vgaSetColor VGALightGreen VGABlack
                vgaPuts "Login successful!\n"
                vgaSetColor VGALightGrey VGABlack
                vgaPuts "\n"
            else do
                vgaSetColor VGARed VGABlack
                vgaPuts "Invalid username or password.\n"
                vgaSetColor VGALightGrey VGABlack
                vgaPuts "\n"
                attempt
