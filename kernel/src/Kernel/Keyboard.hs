{-|
Module      : Kernel.Keyboard
Description : PS/2 keyboard driver
Copyright   : (c) LOS Project, 2026
License     : BSD3

PS/2 keyboard input handling.
-}
module Kernel.Keyboard
    ( keyboardInit
    , keyboardGetChar
    , keyboardHasChar
    ) where

import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Kernel.IO

-- | Keyboard I/O ports
keyboardDataPort, keyboardStatusPort :: Word16
keyboardDataPort   = 0x60
keyboardStatusPort = 0x64

-- | Status flags
keyboardOutputFull :: Word8
keyboardOutputFull = 0x01

-- | US keyboard scancode to ASCII (set 1, no shift)
scancodeToAscii :: Word8 -> Char
scancodeToAscii sc = case sc of
    0x02 -> '1'; 0x03 -> '2'; 0x04 -> '3'; 0x05 -> '4'; 0x06 -> '5'
    0x07 -> '6'; 0x08 -> '7'; 0x09 -> '8'; 0x0A -> '9'; 0x0B -> '0'
    0x0C -> '-'; 0x0D -> '='; 0x0E -> '\b'  -- backspace
    0x0F -> '\t'  -- tab
    0x10 -> 'q'; 0x11 -> 'w'; 0x12 -> 'e'; 0x13 -> 'r'; 0x14 -> 't'
    0x15 -> 'y'; 0x16 -> 'u'; 0x17 -> 'i'; 0x18 -> 'o'; 0x19 -> 'p'
    0x1A -> '['; 0x1B -> ']'; 0x1C -> '\n'  -- enter
    0x1E -> 'a'; 0x1F -> 's'; 0x20 -> 'd'; 0x21 -> 'f'; 0x22 -> 'g'
    0x23 -> 'h'; 0x24 -> 'j'; 0x25 -> 'k'; 0x26 -> 'l'; 0x27 -> ';'
    0x28 -> '\''; 0x29 -> '`'
    0x2B -> '\\'; 0x2C -> 'z'; 0x2D -> 'x'; 0x2E -> 'c'; 0x2F -> 'v'
    0x30 -> 'b'; 0x31 -> 'n'; 0x32 -> 'm'; 0x33 -> ','; 0x34 -> '.'
    0x35 -> '/'; 0x39 -> ' '  -- space
    _    -> '\0'

-- | Shifted versions
scancodeToAsciiShift :: Word8 -> Char
scancodeToAsciiShift sc = case sc of
    0x02 -> '!'; 0x03 -> '@'; 0x04 -> '#'; 0x05 -> '$'; 0x06 -> '%'
    0x07 -> '^'; 0x08 -> '&'; 0x09 -> '*'; 0x0A -> '('; 0x0B -> ')'
    0x0C -> '_'; 0x0D -> '+'; 0x0E -> '\b'
    0x0F -> '\t'
    0x10 -> 'Q'; 0x11 -> 'W'; 0x12 -> 'E'; 0x13 -> 'R'; 0x14 -> 'T'
    0x15 -> 'Y'; 0x16 -> 'U'; 0x17 -> 'I'; 0x18 -> 'O'; 0x19 -> 'P'
    0x1A -> '{'; 0x1B -> '}'; 0x1C -> '\n'
    0x1E -> 'A'; 0x1F -> 'S'; 0x20 -> 'D'; 0x21 -> 'F'; 0x22 -> 'G'
    0x23 -> 'H'; 0x24 -> 'J'; 0x25 -> 'K'; 0x26 -> 'L'; 0x27 -> ':'
    0x28 -> '"'; 0x29 -> '~'
    0x2B -> '|'; 0x2C -> 'Z'; 0x2D -> 'X'; 0x2E -> 'C'; 0x2F -> 'V'
    0x30 -> 'B'; 0x31 -> 'N'; 0x32 -> 'M'; 0x33 -> '<'; 0x34 -> '>'
    0x35 -> '?'; 0x39 -> ' '
    _    -> '\0'

-- | Left and right shift scancodes
leftShiftPress, rightShiftPress :: Word8
leftShiftPress  = 0x2A
rightShiftPress = 0x36

leftShiftRelease, rightShiftRelease :: Word8
leftShiftRelease  = 0xAA
rightShiftRelease = 0xB6

-- | Shift state (stored in kernel memory)
shiftStatePtr :: Ptr Word8
shiftStatePtr = wordPtrToPtr 0x100020

getShiftState :: IO Bool
getShiftState = do
    v <- peek shiftStatePtr
    return (v /= 0)

setShiftState :: Bool -> IO ()
setShiftState b = poke shiftStatePtr (if b then 1 else 0)

-- | Initialize keyboard
keyboardInit :: IO ()
keyboardInit = do
    -- Wait for keyboard controller to be ready
    waitReady
    setShiftState False
  where
    waitReady = do
        status <- inb keyboardStatusPort
        if status .&. 0x02 /= 0
            then waitReady
            else return ()

-- | Check if a key is available
keyboardHasChar :: IO Bool
keyboardHasChar = do
    status <- inb keyboardStatusPort
    return $ (status .&. keyboardOutputFull) /= 0

-- | Read a character (blocking)
keyboardGetChar :: IO Char
keyboardGetChar = loop
  where
    loop = do
        -- Wait for key press
        hasChar <- keyboardHasChar
        if not hasChar
            then loop
            else do
                scancode <- inb keyboardDataPort

                -- Key release (bit 7 set)
                if scancode .&. 0x80 /= 0
                    then do
                        let released = scancode .&. 0x7F
                        if released == leftShiftPress || released == rightShiftPress
                            then setShiftState False
                            else return ()
                        loop
                    else do
                        -- Shift press
                        if scancode == leftShiftPress || scancode == rightShiftPress
                            then do
                                setShiftState True
                                loop
                            else do
                                -- Convert to ASCII
                                shifted <- getShiftState
                                let c = if shifted
                                        then scancodeToAsciiShift scancode
                                        else scancodeToAscii scancode
                                if c /= '\0'
                                    then return c
                                    else loop
