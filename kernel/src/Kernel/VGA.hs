{-# LANGUAGE MagicHash #-}

{-|
Module      : Kernel.VGA
Description : VGA text mode driver
Copyright   : (c) LOS Project, 2026
License     : BSD3

VGA text mode output for the kernel console.
Memory-mapped at 0xB8000.
-}
module Kernel.VGA
    ( -- * Colors
      VGAColor(..)
      -- * Initialization
    , vgaInit
    , vgaClear
      -- * Output
    , vgaPutChar
    , vgaPuts
      -- * Attributes
    , vgaSetColor
    ) where

import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable

-- | VGA text mode colors
data VGAColor
    = VGABlack
    | VGABlue
    | VGAGreen
    | VGACyan
    | VGARed
    | VGAMagenta
    | VGABrown
    | VGALightGrey
    | VGADarkGrey
    | VGALightBlue
    | VGALightGreen
    | VGALightCyan
    | VGALightRed
    | VGALightMagenta
    | VGALightBrown
    | VGAWhite
    deriving (Eq, Enum)

-- | VGA constants
vgaWidth, vgaHeight :: Int
vgaWidth  = 80
vgaHeight = 25

vgaMemory :: Ptr Word16
vgaMemory = wordPtrToPtr 0xB8000

-- | Current cursor position and color (stored in kernel memory)
-- In a real implementation, these would be proper mutable state
-- For now, using unsafePerformIO with IORefs

{-# NOINLINE vgaRow #-}
{-# NOINLINE vgaCol #-}
{-# NOINLINE vgaColorAttr #-}

-- Simple mutable state using pointers at fixed addresses
-- In a real kernel, we'd use proper kernel memory management

-- State storage (simplified - uses first available kernel memory)
stateBase :: Ptr Word8
stateBase = wordPtrToPtr 0x100000  -- 1MB mark

vgaRowPtr :: Ptr Int
vgaRowPtr = castPtr stateBase

vgaColPtr :: Ptr Int
vgaColPtr = castPtr $ stateBase `plusPtr` 8

vgaColorPtr :: Ptr Word8
vgaColorPtr = stateBase `plusPtr` 16

vgaRow :: IO Int
vgaRow = peek vgaRowPtr

vgaCol :: IO Int
vgaCol = peek vgaColPtr

vgaColorAttr :: IO Word8
vgaColorAttr = peek vgaColorPtr

setVgaRow :: Int -> IO ()
setVgaRow = poke vgaRowPtr

setVgaCol :: Int -> IO ()
setVgaCol = poke vgaColPtr

setVgaColorAttr :: Word8 -> IO ()
setVgaColorAttr = poke vgaColorPtr

-- | Convert color to byte
colorToByte :: VGAColor -> Word8
colorToByte = fromIntegral . fromEnum

-- | Make color attribute from foreground and background
makeColor :: VGAColor -> VGAColor -> Word8
makeColor fg bg = colorToByte fg .|. (colorToByte bg `shiftL` 4)

-- | Make VGA entry (character + attribute)
makeEntry :: Char -> Word8 -> Word16
makeEntry c color = fromIntegral (fromEnum c) .|. (fromIntegral color `shiftL` 8)

-- | Initialize VGA
vgaInit :: IO ()
vgaInit = do
    setVgaColorAttr (makeColor VGALightGrey VGABlack)
    vgaClear

-- | Clear the screen
vgaClear :: IO ()
vgaClear = do
    color <- vgaColorAttr
    let entry = makeEntry ' ' color
    mapM_ (\i -> pokeElemOff vgaMemory i entry) [0 .. vgaWidth * vgaHeight - 1]
    setVgaRow 0
    setVgaCol 0

-- | Set text color
vgaSetColor :: VGAColor -> VGAColor -> IO ()
vgaSetColor fg bg = setVgaColorAttr (makeColor fg bg)

-- | Scroll the screen up by one line
vgaScroll :: IO ()
vgaScroll = do
    -- Move all lines up
    mapM_ copyLine [0 .. vgaHeight - 2]
    -- Clear last line
    color <- vgaColorAttr
    let entry = makeEntry ' ' color
    mapM_ (\x -> pokeElemOff vgaMemory ((vgaHeight - 1) * vgaWidth + x) entry)
          [0 .. vgaWidth - 1]
    setVgaRow (vgaHeight - 1)
  where
    copyLine y = mapM_ (copyChar y) [0 .. vgaWidth - 1]
    copyChar y x = do
        entry <- peekElemOff vgaMemory ((y + 1) * vgaWidth + x)
        pokeElemOff vgaMemory (y * vgaWidth + x) entry

-- | Put a single character
vgaPutChar :: Char -> IO ()
vgaPutChar '\n' = do
    setVgaCol 0
    row <- vgaRow
    if row >= vgaHeight - 1
        then vgaScroll
        else setVgaRow (row + 1)

vgaPutChar '\r' = setVgaCol 0

vgaPutChar '\t' = do
    col <- vgaCol
    let newCol = (col + 8) .&. complement 7
    if newCol >= vgaWidth
        then do
            setVgaCol 0
            row <- vgaRow
            if row >= vgaHeight - 1
                then vgaScroll
                else setVgaRow (row + 1)
        else setVgaCol newCol

vgaPutChar c = do
    row <- vgaRow
    col <- vgaCol
    color <- vgaColorAttr
    pokeElemOff vgaMemory (row * vgaWidth + col) (makeEntry c color)
    let newCol = col + 1
    if newCol >= vgaWidth
        then do
            setVgaCol 0
            if row >= vgaHeight - 1
                then vgaScroll
                else setVgaRow (row + 1)
        else setVgaCol newCol

-- | Put a string
vgaPuts :: String -> IO ()
vgaPuts = mapM_ vgaPutChar
