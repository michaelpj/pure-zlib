{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -dumpdir /tmp/dumps #-}
module Codec.Compression.Zlib.HuffmanTree(
         HuffmanTree
       , AdvanceResult(..)
       , createHuffmanTree
       , advanceTree
       )
 where

import Data.Bits(testBit)
import Data.Word(Word8)

data HuffmanTree = HuffmanNode !HuffmanTree !HuffmanTree
                 | HuffmanValue Int
                 | HuffmanEmpty
 deriving (Show)

data AdvanceResult = AdvanceError String
                   | NewTree HuffmanTree
                   | Result Int

emptyHuffmanTree :: HuffmanTree
emptyHuffmanTree = HuffmanEmpty

createHuffmanTree :: [(Int, Int, Int)] ->
                     Either String HuffmanTree
createHuffmanTree = foldr addHuffmanNode' (Right emptyHuffmanTree)
 where addHuffmanNode' (a, b, c) acc =
         case acc of
           Left err   -> Left err
           Right tree -> addHuffmanNode a c b tree

addHuffmanNode :: Int -> Int -> Int -> HuffmanTree ->
                  Either String HuffmanTree
addHuffmanNode !val !code = go
  where
    go !len !node =
      case node of
        HuffmanEmpty | len == 0  -> Right (HuffmanValue val)
        HuffmanEmpty | otherwise ->
          case go (len - 1) HuffmanEmpty of
            Left err -> Left err
            Right newNode
              | testBit code (len - 1) -> Right (HuffmanNode HuffmanEmpty newNode)
              | otherwise              -> Right (HuffmanNode newNode HuffmanEmpty)
        --
        HuffmanValue _  | len == 0  -> Left "Two values point to the same place!"
        HuffmanValue _  | otherwise -> Left "HuffmanValue hit while inserting a value!"
        --
        HuffmanNode _ _ | len == 0 -> Left ("Tried to add where the leaf is a node: " ++ show val)
        HuffmanNode l r | testBit code (len - 1) ->
          case go (len - 1) r of
            Left err -> Left err
            Right r' -> Right (HuffmanNode l r')
        HuffmanNode l r | otherwise ->
          case go (len - 1) l of
            Left err -> Left err
            Right l' -> Right (HuffmanNode l' r)

advanceTree :: Word8 -> HuffmanTree -> AdvanceResult
advanceTree !x node =
  case node of
    HuffmanEmpty     -> AdvanceError "Tried to advance empty tree!"
    HuffmanValue _   -> AdvanceError "Tried to advance value!"
    HuffmanNode  l r ->
      case if (x == 1) then r else l of
        HuffmanEmpty   -> AdvanceError "Advanced to empty tree!"
        HuffmanValue y -> Result y
        t              -> NewTree t
{-# INLINE advanceTree #-}
