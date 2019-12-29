module Pure.Covariance (Covariance,covary,covaries,covariance,count,meanx,meany,meanx2,meany2,c,variance_x,variance_y,stdDev_x,stdDev_y,correlation) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Variance (Vary())

import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Covariance = Covariance
  { cCount    :: {-# UNPACK #-} !Double
  , cMeanx    :: {-# UNPACK #-} !Double
  , cMeany    :: {-# UNPACK #-} !Double
  , cMeanx2   :: {-# UNPACK #-} !Double
  , cMeany2   :: {-# UNPACK #-} !Double
  , cC        :: {-# UNPACK #-} !Double
  } deriving (Vary,Eq,Ord,Generic,ToJSON,FromJSON,Show)

instance Monoid Covariance where
  {-# INLINE mempty #-}
  mempty = Covariance 0 0 0 0 0 0

instance Semigroup Covariance where
  {-# INLINE (<>) #-}
  (<>) c1 c2
    | cCount c1 == 0 = c2
    | cCount c2 == 0 = c1
    | otherwise =
      let
        n1 = cCount c1
        n2 = cCount c2

        mx1 = cMeanx c1
        mx2 = cMeanx c2

        my1 = cMeany c1
        my2 = cMeany c2

        dx = mx1 - mx2
        dy = my1 - my2

        count = n1 + n2

        meanx = (n1 * mx1 + n2 * mx2) / count

        meany = (n1 * my1 + n2 * my2) / count

        meanx2 = v_x c1 + v_x c2 + dx ^^ 2 * n1 * n2 / count
        meany2 = v_y c1 + v_y c2 + dy ^^ 2 * n1 * n2 / count

        c = cC c1 + cC c2 + (mx1 - mx2) * (my1 - my2) * (n1 * n2 / count)

        v_x c
          | cCount c < 2 = 0
          | otherwise    = cMeanx2 c

        v_y c
          | cCount c < 2 = 0
          | otherwise    = cMeany2 c
       in
        Covariance count meanx meany meanx2 meany2 c

count :: Covariance -> Int
count = round . cCount

meanx :: Covariance -> Maybe Double
meanx c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeanx c)

meanx2 :: Covariance -> Maybe Double
meanx2 c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeanx2 c)

meany :: Covariance -> Maybe Double
meany c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeany c)

meany2 :: Covariance -> Maybe Double
meany2 c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeany2 c)

c :: Covariance -> Maybe Double
c cov
  | cCount cov == 0 = Nothing
  | otherwise       = Just (cC cov)

{-# RULES
"covary f g a mempty" forall f g a. covary f g a (Covariance 0 0 0 0 0 0) = let { x = realToFrac (f a); y = realToFrac (g a) } in Covariance 1 x y 0 0 0
  #-}

{-# INLINE covary #-}
covary :: (Real x, Real y) => (a -> x) -> (a -> y) -> a -> Covariance -> Covariance
covary f g a Covariance {..} =
  let
    x = realToFrac (f a)
    y = realToFrac (g a)

    dx = x - cMeanx
    dy = y - cMeany

    count = cCount + 1
    meanx = cMeanx + dx / count
    meany = cMeany + dy / count
    meanx2 = cMeanx2 + dx * (x - meanx)
    meany2 = cMeany2 + dy * (y - meany)
    c = cC + dx * (y - meany) -- NOTE: this looks instictively wrong, but isn't

   in
    Covariance count meanx meany meanx2 meany2 c

{-# INLINE covaries #-}
covaries :: (Foldable f, Real x, Real y) => (a -> x) -> (a -> y) -> f a -> Covariance
covaries f g = Foldable.foldl' (flip (covary f g)) mempty

{-# INLINE covariance #-}
covariance :: Covariance -> Maybe Double
covariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / (cCount - 1)

{-# INLINE variance_x #-}
variance_x :: Covariance -> Maybe Double
variance_x Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / (cCount - 1)

{-# INLINE variance_y #-}
variance_y :: Covariance -> Maybe Double
variance_y Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / (cCount - 1)

{-# INLINE stdDev_x #-}
stdDev_x :: Covariance -> Maybe Double
stdDev_x = fmap sqrt . variance_x

{-# INLINE stdDev_y #-}
stdDev_y :: Covariance -> Maybe Double
stdDev_y = fmap sqrt . variance_y

-- linear correlation; Pearson
{-# INLINE correlation #-}
correlation :: Covariance -> Maybe Double
correlation c = do
  cov <- covariance c
  sdx <- stdDev_x c
  sdy <- stdDev_y c
  pure $ cov / (sdx * sdy)