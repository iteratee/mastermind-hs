{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GuessThread
  ( guessThread,
    guessThreadPickColors,
  )
where

import CheckGuess
import Control.Category ((>>>))
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString (parse)
import Data.Attoparsec.ByteString qualified as AP (IResult (..), Result)
import Data.ByteString.Builder
  ( Builder,
    byteString,
    toLazyByteString,
  )
import Data.ByteString.Char8 (ByteString, empty, null)
import Data.Vector.Unboxed (fromList, length)
import Parser
import Serialize
import System.Random.PCG (createSystemRandom)
import System.Random.PCG.Class (Generator (..), uniformR)
import System.Socket (Socket, close, receive)
import System.Socket.Type.Stream (Stream, sendAllLazy)
import Types
import Prelude hiding (length, null)

crlf :: Builder
crlf = byteString "\r\n"

pickColors :: Generator g m => Int -> Int -> g -> m Colors
pickColors colors pegs =
  fmap fromList . replicateM pegs . fmap Color . uniformR (0, colors - 1)

guessThreadPickColors :: Int -> Int -> Socket f Stream p -> IO ()
guessThreadPickColors nColors nPegs socket = do
  colors <- pickColors nColors nPegs =<< createSystemRandom
  guessThread colors socket

guessThread :: Colors -> Socket f Stream p -> IO ()
guessThread secret = guessThreadRemainder secret empty

guessThreadRemainder :: Colors -> ByteString -> Socket f Stream p -> IO ()
guessThreadRemainder secret remainder socket = do
  let maxCorrect :: Word = fromIntegral $ length secret
      closeIfEmptyElse :: (ByteString -> IO (Maybe a)) -> ByteString -> IO (Maybe a)
      closeIfEmptyElse elseHandler bs
        | null bs = close socket >> return Nothing
        | otherwise = elseHandler bs
      parseLoop :: ByteString -> IO (Maybe (Colors, ByteString))
      parseLoop parseRemainder =
        if null parseRemainder
          then receive socket 4096 mempty >>= closeIfEmptyElse parseLoop
          else parseResultLoop (parse parseColors parseRemainder)
      parseResultLoop :: AP.Result Colors -> IO (Maybe (Colors, ByteString))
      parseResultLoop (AP.Done newRemainder colors) = return $ Just (colors, newRemainder)
      parseResultLoop (AP.Fail _remainder _context _message) = return Nothing
      parseResultLoop (AP.Partial cont) =
        receive socket 4096 mempty >>= closeIfEmptyElse (cont >>> parseResultLoop)
  maybeGuessResult <- parseLoop remainder
  case maybeGuessResult of
    Nothing -> close socket
    Just (guess, newRemainder) ->
      let result = checkGuess secret guess
          textResult = encodeResult result
          toSend = toLazyByteString (textResult <> crlf)
       in do
            _ <- sendAllLazy socket toSend mempty
            if result == allCorrect maxCorrect
              then close socket
              else guessThreadRemainder secret newRemainder socket
