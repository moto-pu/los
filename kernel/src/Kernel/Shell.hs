{-|
Module      : Kernel.Shell
Description : Simple command shell
Copyright   : (c) LOS Project, 2026
License     : BSD3

A minimal shell for the LOS kernel.
Demonstrates the effect system concepts in a real-world context.
-}
module Kernel.Shell
    ( shellMain
    ) where

import Kernel.VGA
import Kernel.Keyboard
import Data.Char (toLower)

-- | Shell prompt
prompt :: String
prompt = "los> "

-- | Read a line from keyboard
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
                    vgaPuts "\b \b"
                    go rest
            _ -> do
                vgaPutChar c
                go (c : acc)

-- | Parse command and arguments
parseCommand :: String -> (String, [String])
parseCommand input =
    case words input of
        []     -> ("", [])
        (c:as) -> (map toLower c, as)

-- | Execute a command
executeCommand :: String -> [String] -> IO Bool  -- Returns False to exit
executeCommand cmd args = case cmd of
    ""       -> return True  -- Empty command
    "help"   -> cmdHelp >> return True
    "clear"  -> cmdClear >> return True
    "echo"   -> cmdEcho args >> return True
    "whoami" -> cmdWhoami >> return True
    "uname"  -> cmdUname >> return True
    "lambda" -> cmdLambda >> return True
    "exit"   -> cmdExit >> return False
    "halt"   -> cmdHalt >> return False
    _        -> cmdUnknown cmd >> return True

-- | Help command
cmdHelp :: IO ()
cmdHelp = do
    vgaPuts "Available commands:\n"
    vgaPuts "  help    - Show this help message\n"
    vgaPuts "  clear   - Clear the screen\n"
    vgaPuts "  echo    - Print arguments\n"
    vgaPuts "  whoami  - Show current user\n"
    vgaPuts "  uname   - Show system information\n"
    vgaPuts "  lambda  - Show lambda calculus trivia\n"
    vgaPuts "  exit    - Exit shell\n"
    vgaPuts "  halt    - Halt the system\n"

-- | Clear command
cmdClear :: IO ()
cmdClear = vgaClear

-- | Echo command
cmdEcho :: [String] -> IO ()
cmdEcho args = do
    vgaPuts (unwords args)
    vgaPuts "\n"

-- | Whoami command
cmdWhoami :: IO ()
cmdWhoami = vgaPuts "root\n"

-- | Uname command
cmdUname :: IO ()
cmdUname = do
    vgaPuts "LOS 0.1.0 (Lambda Operating System)\n"
    vgaPuts "Architecture: x86\n"
    vgaPuts "Kernel: Effect-based microkernel\n"

-- | Lambda calculus trivia
cmdLambda :: IO ()
cmdLambda = do
    vgaSetColor VGALightCyan VGABlack
    vgaPuts "Lambda Calculus Facts:\n"
    vgaSetColor VGALightGrey VGABlack
    vgaPuts "  - Invented by Alonzo Church in the 1930s\n"
    vgaPuts "  - Foundation of functional programming\n"
    vgaPuts "  - Only three constructs: variable, abstraction, application\n"
    vgaPuts "  - Turing-complete: can compute anything computable\n"
    vgaPuts "  - LOS uses effects to model OS operations\n"
    vgaPuts "\n"
    vgaSetColor VGALightGreen VGABlack
    vgaPuts "  (\\x. x) is the identity function\n"
    vgaSetColor VGALightGrey VGABlack

-- | Exit command
cmdExit :: IO ()
cmdExit = vgaPuts "Goodbye!\n"

-- | Halt command
cmdHalt :: IO ()
cmdHalt = do
    vgaSetColor VGALightRed VGABlack
    vgaPuts "System halting...\n"
    vgaSetColor VGALightGrey VGABlack

-- | Unknown command
cmdUnknown :: String -> IO ()
cmdUnknown cmd = do
    vgaSetColor VGARed VGABlack
    vgaPuts $ "Unknown command: " ++ cmd ++ "\n"
    vgaSetColor VGALightGrey VGABlack
    vgaPuts "Type 'help' for available commands.\n"

-- | Main shell loop
shellMain :: IO ()
shellMain = do
    vgaPuts "Welcome to LOS Shell\n"
    vgaPuts "Type 'help' for available commands.\n\n"
    loop
  where
    loop = do
        vgaSetColor VGALightGreen VGABlack
        vgaPuts prompt
        vgaSetColor VGALightGrey VGABlack
        input <- readLine
        let (cmd, args) = parseCommand input
        continue <- executeCommand cmd args
        if continue
            then loop
            else return ()
