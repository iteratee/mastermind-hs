{-# LANGUAGE NamedFieldPuns #-}
module Serialize
  ( encodeResult
  ) where

import Data.ByteString.Builder

import Types

encodeResult :: Result -> Builder
encodeResult Result {totalCorrectColors, totalCorrectPlaces} =
  wordDec totalCorrectPlaces <> char8 ' ' <> wordDec totalCorrectColors
