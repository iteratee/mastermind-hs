{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Types
  ( Color(..)
  , Colors
  , Result(..)
  ) where

import Data.Word                      (Word)
import Data.Vector.Unboxed         as U
import Data.Vector.Generic         as G
import Data.Vector.Generic.Mutable as M

newtype Color = Color { unColor :: Int }
  deriving (Eq, Ord, Show)

newtype instance U.MVector s Color = MVColor (U.MVector s Int)
unMVColor :: forall s. U.MVector s Color -> U.MVector s Int
unMVColor (MVColor mvi) = mvi
newtype instance U.Vector    Color = VColor  (U.Vector    Int)
unVColor :: U.Vector Color -> U.Vector Int
unVColor (VColor vi) = vi
instance M.MVector U.MVector Color where
  basicLength = M.basicLength . unMVColor
  {-# INLINE basicLength #-}
  basicOverlaps (MVColor mvi1) (MVColor mvi2) = M.basicOverlaps mvi1 mvi2
  {-# INLINE basicOverlaps #-}
  basicUnsafeSlice n m = MVColor . M.basicUnsafeSlice n m . unMVColor
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeNew = fmap MVColor . M.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}
  basicInitialize = M.basicInitialize . unMVColor
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MVColor mvi) = fmap Color . M.basicUnsafeRead mvi
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MVColor mvi) n = M.basicUnsafeWrite mvi n . unColor
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeReplicate n (Color {unColor = intVal}) =
    MVColor <$> M.basicUnsafeReplicate n intVal
instance G.Vector U.Vector Color where
  basicUnsafeIndexM (VColor vi) = fmap Color . G.basicUnsafeIndexM vi
  {-# INLINE basicUnsafeIndexM #-}
  basicLength = G.basicLength . unVColor
  {-# INLINE basicLength #-}
  basicUnsafeFreeze = fmap VColor . G.basicUnsafeFreeze . unMVColor
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw = fmap MVColor . G.basicUnsafeThaw . unVColor
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeSlice n m = VColor . G.basicUnsafeSlice n m . unVColor
  {-# INLINE basicUnsafeSlice #-}

instance Unbox Color

type Colors = U.Vector Color

data Result = Result
  { totalCorrectColors :: !Word
  , totalCorrectPlaces :: !Word
  } deriving (Eq, Ord, Show)
