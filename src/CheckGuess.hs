{-# LANGUAGE BangPatterns #-}

module CheckGuess
  ( allCorrect,
    checkGuess,
  )
where

import Control.Monad.ST.Strict (ST (..), runST)
import Data.Monoid (Sum (..))
import Data.Vector.Algorithms.Insertion (sort)
import Data.Vector.Generic (basicUnsafeFreeze)
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed
  ( MVector,
    Vector,
    foldMap',
    length,
    thaw,
    unsafeThaw,
    zipWith,
    (!),
  )
import Types
import Prelude hiding (length, zipWith)

countCorrectColors :: Colors -> Colors -> Word
countCorrectColors sortedSource sortedGuess =
  go (length sortedSource - 1) (length sortedGuess - 1) 0
  where
    go :: Int -> Int -> Word -> Word
    go (-1) !_ !count = count
    go !_ (-1) !count = count
    go !sourceIdx !guessIdx !count
      | sourceIdx >= length sortedSource = count
      | guessIdx >= length sortedGuess = count
      | otherwise =
          let sX = sortedSource ! sourceIdx
              gX = sortedGuess ! guessIdx
           in case compare sX gX of
                LT -> go sourceIdx (guessIdx - 1) count
                GT -> go (sourceIdx - 1) guessIdx count
                EQ -> go (sourceIdx - 1) (guessIdx - 1) (count + 1)

checkGuessST ::
  forall s. Vector Color -> Vector Color -> ST s Result
checkGuessST source guess = do
  (mSource :: MVector s Color) <- thaw source
  (mGuess :: MVector s Color) <- thaw guess
  let countTrue :: Vector Bool -> Word
      countTrue = getSum . foldMap' (Sum . fromIntegral . fromEnum)
      !correctPlaces = countTrue $ zipWith (==) source guess
  sort mSource
  sort mGuess
  source' <- basicUnsafeFreeze mSource
  guess' <- basicUnsafeFreeze mGuess
  let !correctColors = countCorrectColors source' guess'
  return Result {totalCorrectColors = correctColors, totalCorrectPlaces = correctPlaces}

checkGuess :: Colors -> Colors -> Result
checkGuess source guess =
  if length source == length guess
    then runST $ checkGuessST source guess
    else Result {totalCorrectColors = 0, totalCorrectPlaces = 0}

allCorrect :: Word -> Result
allCorrect total =
  Result
    { totalCorrectColors = total,
      totalCorrectPlaces = total
    }
