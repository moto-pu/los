{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Prelude hiding (getLine)
import qualified Data.Map.Strict as Map
import LOS.Effect.Core
import LOS.Effect.Console
import LOS.Effect.FileSystem
import LOS.Effect.State

-- | Simple test runner
test :: String -> Bool -> IO ()
test name passed = putStrLn $ (if passed then "✓ " else "✗ ") ++ name

-- | Test: Pure console effect
testConsole :: Bool
testConsole =
    let program = do
            putLine "Hello"
            name <- getLine
            putLine ("Hi " ++ name)
        (output, ()) = run $ runConsolePure ["World"] program
    in output == ["Hello", "Hi World"]

-- | Test: State effect
testState :: Bool
testState =
    let program = do
            put (10 :: Int)
            modify (+5)
            get
        (finalState, result) = run $ runState 0 program
    in finalState == 15 && result == 15

-- | Test: Combined effects (Console + State)
testCombined :: Bool
testCombined =
    let program :: (Member Console effs, Member (State Int) effs) => Eff effs Int
        program = do
            putLine "Start"
            put (1 :: Int)
            putLine "Middle"
            modify (+1)
            putLine "End"
            get
        -- Handle State first, then Console
        (output, (finalState, result)) =
            run $ runConsolePure [] $ runState 0 program
    in output == ["Start", "Middle", "End"]
       && finalState == 2
       && result == 2

-- | Test: FileSystem effect with virtual FS
testFileSystem :: Bool
testFileSystem =
    let initialFS = Map.fromList [("test.txt", "Hello")]
        program = do
            content <- readFile "test.txt"
            writeFile "output.txt" (content ++ " World")
            readFile "output.txt"
        (finalFS, result) = run $ runFileSystemPure initialFS program
    in result == "Hello World"
       && Map.lookup "output.txt" finalFS == Just "Hello World"

main :: IO ()
main = do
    putStrLn "=== LOS Effect System Tests ==="
    putStrLn ""
    test "Console effect with pure handler" testConsole
    test "State effect" testState
    test "Combined Console + State effects" testCombined
    test "FileSystem effect with virtual FS" testFileSystem
    putStrLn ""
    putStrLn "Done."
