{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec

import Data.Attoparsec.ByteString (Result, IResult(..), parse)
import Data.Vector.Unboxed (fromList)

import Parser
import Types

makeColors :: [Int] -> Colors
makeColors = fromList . map Color

spec :: Spec
spec = do
  describe "parseColor" $ do
    it "parses decimal values" $ do
      parse parseColor "7 " `shouldSatisfy` \case
          Done " " (Color 7) -> True
          _                  -> False
    it "doesn't parse negative values" $ do
      parse parseColor "-7" `shouldSatisfy` \case
          Fail "-7" _ _ -> True
          _             -> False
    it "doesn't parse + prefixed values" $ do
      parse parseColor "+7" `shouldSatisfy` \case
          Fail "+7" _ _ -> True
          _             -> False
    it "parses multiple characters" $ do
      parse parseColor "123456789 " `shouldSatisfy` \case
          Done " " (Color 123456789) -> True
          _                          -> False
    it "waits if the input ends on a digit" $ do
      parse parseColor "1" `shouldSatisfy` \case
          Partial _cont -> True
          _             -> False

  describe "parseColors" $ do
    it "waits on the empty string" $ do
      parse parseColors "" `shouldSatisfy` \case
          Partial _cont -> True
          _             -> False
      parse parseColors "  " `shouldSatisfy` \case
          Partial _cont -> True
          _             -> False
    it "waits if an eol is missing" $ do
      parse parseColors "1 2 3 " `shouldSatisfy` \case
          Partial _cont -> True
          _             -> False
    it "accepts \r\n as eol" $ do
      parse parseColors "1 2 3\r\n4 5 6" `shouldSatisfy` \case
          Partial _cont -> True
          Done "4 5 6" colors -> colors == makeColors [1, 2, 3]
          _                   -> False
    it "accepts \n as eol" $ do
      parse parseColors "1 2 3\n4 5 6" `shouldSatisfy` \case
          Done "4 5 6" colors -> colors == makeColors [1, 2, 3]
          _                   -> False
    it "doesn't accept \r as eol" $ do
      parse parseColors "1 2 3\r4 5 6" `shouldSatisfy` \case
          Fail "\r4 5 6" _ _ -> True
          _                  -> False
    it "accepts eof to end a color string" $ do
      case parse parseColors "1 2 3" of
        Partial cont ->
          case cont "" of
            Done "" colors -> colors `shouldBe` makeColors [1, 2, 3]
            _ -> expectationFailure "Expected feeding EOF to parser to succeed"
        _ -> expectationFailure "Expected passing \"1 2 3\" to parseColors to yield Partial"
