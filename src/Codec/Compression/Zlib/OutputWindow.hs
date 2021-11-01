{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Compression.Zlib.OutputWindow (
  OutputWindow,
  emptyWindow,
  emitExcess,
  finalizeWindow,
  addByte,
  addChunk,
  addOldChunk,
) where

import Control.Monad (foldM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.Vector.Storable.ByteString
import GHC.ST (ST (..))
import Foreign
import Control.Monad.ST.Unsafe

windowSize :: Int
windowSize = 128 * 1024

data OutputWindow s = OutputWindow
  { owWindow :: {-# UNPACK #-} !(MV.MVector s Word8)
  , owNext :: {-# UNPACK #-} !Int
  }

emptyWindow :: ST s (OutputWindow s)
emptyWindow = do
  window <- MV.new windowSize
  return (OutputWindow window 0)

excessChunkSize :: Int
excessChunkSize = 32768

emitExcess :: OutputWindow s -> ST s (Maybe (S.ByteString, OutputWindow s))
emitExcess OutputWindow{owWindow = window, owNext = initialOffset}
  | initialOffset < excessChunkSize * 2 = return Nothing
  | otherwise = do
    toEmit <- V.freeze $ MV.slice 0 excessChunkSize window
    let excessLength = initialOffset - excessChunkSize
    -- Need move as these can overlap!
    MV.move (MV.slice 0 excessLength window) (MV.slice excessChunkSize excessLength window)
    let ow' = OutputWindow window excessLength
    return (Just (vectorToByteString toEmit, ow'))

finalizeWindow :: OutputWindow s -> ST s S.ByteString
finalizeWindow ow = do
  -- safe as we're doing it at the end
  res <- V.unsafeFreeze (MV.slice 0 (owNext ow) (owWindow ow))
  pure $ vectorToByteString res

-- -----------------------------------------------------------------------------

addByte :: OutputWindow s -> Word8 -> ST s (OutputWindow s)
addByte !ow !b = do
  let offset = owNext ow
  MV.write (owWindow ow) offset b
  return ow{owNext = offset + 1}

addChunk :: OutputWindow s -> L.ByteString -> ST s (OutputWindow s)
addChunk !ow !bs = foldM copyChunk ow (L.toChunks bs)

copyChunk :: OutputWindow s -> S.ByteString -> ST s (OutputWindow s)
copyChunk ow sbstr = do
  -- safe as we're never going to look at this again
  let ba = byteStringToVector sbstr
      offset = owNext ow
      len = V.length ba
  V.copy (MV.slice offset len (owWindow ow)) ba
  return ow{owNext = offset + len}

addOldChunk :: OutputWindow s -> Int -> Int -> ST s (OutputWindow s, S.ByteString)
addOldChunk (OutputWindow window next) dist len = do
  -- zlib can ask us to copy an "old" chunk that extends past our current offset.
  -- The intention is that we then start copying the "new" data we just copied into
  -- place. This is handled by 'evilCopy'
  evilCopy (MV.slice next len window) (MV.slice (next - dist) len window) len
  result <- V.freeze $ MV.slice next len window
  return (OutputWindow window (next + len), vectorToByteString result)

evilCopy :: MV.MVector s Word8 -> MV.MVector s Word8 -> Int -> ST s ()
evilCopy (MV.MVector _ dest) (MV.MVector _ src) len = unsafeIOToST $ withForeignPtr dest $ \dp -> withForeignPtr src $ \sp -> go dp sp len
  where
    sz = sizeOf (undefined :: Word8)
    go :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
    go _ _ 0 = pure ()
    go dp sp toCopy = do
      poke dp =<< peek sp
      go (dp `plusPtr` sz) (sp `plusPtr` sz) (toCopy-1)
