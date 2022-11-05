module CheckGuessSpec where

import Test.Hspec

import Data.Vector.Unboxed (fromList)

import Types
import CheckGuess

emptyResult = Result
  { totalCorrectColors = 0
  , totalCorrectPlaces = 0
  }

makeColors :: [Int] -> Colors
makeColors = fromList . map Color

spec :: Spec
spec = do
  describe "checkGuess" $ do
    it "returns 0 0 for guess of incorrect length" $ do
      let serverColors = makeColors [0..3]
          guessColors = makeColors [0..2]
      checkGuess serverColors guessColors `shouldBe` emptyResult
    it "returns n n for correct guess" $ do
      let serverColors = makeColors [0..3]
      checkGuess serverColors serverColors `shouldBe` allCorrect 4
    it "calculates correct places" $ do
      let serverColors = makeColors [0,0,0,0]
          guessColors = makeColors [3,1,0,2]
      checkGuess serverColors guessColors `shouldBe` Result
        { totalCorrectColors = 1
        , totalCorrectPlaces = 1
        }
    it "calculates correct colors" $ do
      let serverColors = makeColors [5,4,3,2]
          guessColors = makeColors [3,2,1,0]
      checkGuess serverColors guessColors `shouldBe` Result
        { totalCorrectColors = 2
        , totalCorrectPlaces = 0
        }


