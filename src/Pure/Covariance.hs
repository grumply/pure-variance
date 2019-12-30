module Pure.Covariance
  (Covariance,covary,covaries
  ,count,meanx,meany
  ,covariance,sampleCovariance,populationCovariance
  ,variance_x,sampleVariance_x,populationVariance_x,variance_y,sampleVariance_y,populationVariance_y
  ,stdDev_x,sampleStdDev_x,populationStdDev_x,stdDev_y,sampleStdDev_y,populationStdDev_y
  ,correlation,sampleCorrelation,populationCorrelation
  ,Extract(..),Covaried
  ,lookupCovariance
  ,covaried,covariances
  ) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,fromTxt)
import Pure.Variance (Vary(),varies,sampleVariance,populationVariance,sampleStdDev,populationStdDev)

import Control.Applicative ((<|>))
import Control.Arrow (first)
import qualified Data.Foldable as Foldable
import Data.Functor.Sum
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Data.Int
import Data.List
import Data.Word
import Data.Maybe
import GHC.Generics as G
import GHC.Natural
import GHC.TypeLits

import Data.HashMap.Strict as HM
import Data.Map as Map
import Pure.Data.Txt.Trie as Trie

import qualified Data.Vector.Generic as V

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

meany :: Covariance -> Maybe Double
meany c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeany c)

{-# RULES
"covary f g a mempty == Convariance 1 (realToFrac (f a)) (realToFrac (g a)) 0 0 0" forall f g a. covary f g a (Covariance 0 0 0 0 0 0) = Covariance 1 (realToFrac (f a)) (realToFrac (g a)) 0 0 0
"sampleVariance_x (covaries f g as) == sampleVariance (varies f as)" forall f g as. sampleVariance_x (covaries f g as) = sampleVariance (varies f as)
"populationVariance_x (covaries f g as) == populationVariance (varies f as)" forall f g as. populationVariance_x (covaries f g as) = populationVariance (varies f as)
"sampleVariance_y (covaries f g as) == sampleVariance (varies g as)" forall f g as. sampleVariance_y (covaries f g as) = sampleVariance (varies g as)
"populationVariance_y (covaries f g as) == populationVariance (varies g as)" forall f g as. populationVariance_y (covaries f g as) = populationVariance (varies g as)
"sampleStdDev_x (covaries f g as) == sampleStdDev (varies f as)" forall f g as. sampleStdDev_x (covaries f g as) = sampleStdDev (varies f as)
"populationStdDev_x (covaries f g as) == populationStdDev (varies f as)" forall f g as. populationStdDev_x (covaries f g as) = populationStdDev (varies f as)
"sampleStdDev_y (covaries f g as) == sampleStdDev (varies g as)" forall f g as. sampleStdDev_y (covaries f g as) = sampleStdDev (varies g as)
"populationStdDev_y (covaries f g as) == populationStdDev (varies g as)" forall f g as. populationStdDev_y (covaries f g as) = populationStdDev (varies g as)
  #-}

{-# INLINE [1] covary #-}
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

{-# INLINE [1] covaries #-}
covaries :: (Foldable f, Real x, Real y) => (a -> x) -> (a -> y) -> f a -> Covariance
covaries f g = Foldable.foldl' (flip (covary f g)) mempty

{-# INLINE covariance #-}
covariance :: Covariance -> Maybe Double
covariance = sampleCovariance

{-# INLINE [1] sampleCovariance #-}
sampleCovariance :: Covariance -> Maybe Double
sampleCovariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / (cCount - 1)

{-# INLINE [1] populationCovariance #-}
populationCovariance :: Covariance -> Maybe Double
populationCovariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / cCount

{-# INLINE variance_x #-}
variance_x :: Covariance -> Maybe Double
variance_x = sampleVariance_x

{-# INLINE [1] sampleVariance_x #-}
sampleVariance_x :: Covariance -> Maybe Double
sampleVariance_x Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / (cCount - 1)

{-# INLINE [1] populationVariance_x #-}
populationVariance_x :: Covariance -> Maybe Double
populationVariance_x Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / cCount

{-# INLINE variance_y #-}
variance_y :: Covariance -> Maybe Double
variance_y = sampleVariance_y

{-# INLINE [1] sampleVariance_y #-}
sampleVariance_y :: Covariance -> Maybe Double
sampleVariance_y Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / (cCount - 1)

{-# INLINE [1] populationVariance_y #-}
populationVariance_y :: Covariance -> Maybe Double
populationVariance_y Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / cCount

{-# INLINE stdDev_x #-}
stdDev_x :: Covariance -> Maybe Double
stdDev_x = sampleStdDev_x

{-# INLINE [1] sampleStdDev_x #-}
sampleStdDev_x :: Covariance -> Maybe Double
sampleStdDev_x = fmap sqrt . sampleVariance_x

{-# INLINE [1] populationStdDev_x #-}
populationStdDev_x :: Covariance -> Maybe Double
populationStdDev_x = fmap sqrt . populationVariance_x

{-# INLINE stdDev_y #-}
stdDev_y :: Covariance -> Maybe Double
stdDev_y = sampleStdDev_y

{-# INLINE [1] sampleStdDev_y #-}
sampleStdDev_y :: Covariance -> Maybe Double
sampleStdDev_y = fmap sqrt . sampleVariance_y

{-# INLINE [1] populationStdDev_y #-}
populationStdDev_y :: Covariance -> Maybe Double
populationStdDev_y = fmap sqrt . populationVariance_y

{-# INLINE correlation #-}
correlation :: Covariance -> Maybe Double
correlation = sampleCorrelation

{-# INLINE sampleCorrelation #-}
sampleCorrelation :: Covariance -> Maybe Double
sampleCorrelation c = do
  cov <- sampleCovariance c
  sdx <- sampleStdDev_x c
  sdy <- sampleStdDev_y c
  pure $
    if sdx == 0 || sdy == 0
    then 0
    else cov / (sdx * sdy)

{-# INLINE populationCorrelation #-}
populationCorrelation :: Covariance -> Maybe Double
populationCorrelation c = do
  cov <- populationCovariance c
  sdx <- populationStdDev_x c
  sdy <- populationStdDev_y c
  pure $
    if sdx == 0 || sdy == 0
    then 0
    else cov / (sdx * sdy)

newtype Covaried = Covaried (HashMap (String,String) Covariance)
  deriving (Show,Eq,Generic)

instance Semigroup Covaried where
  {-# INLINE (<>) #-}
  (<>) (Covaried c1) (Covaried c2)
    | HM.null c1 = Covaried c2
    | HM.null c2 = Covaried c1
    | otherwise  = Covaried $ HM.unionWith (<>) c1 c2

instance Monoid Covaried where
  {-# INLINE mempty #-}
  mempty = Covaried mempty

{-# INLINE lookupCovariance #-}
lookupCovariance :: String -> String -> Covaried -> Maybe Covariance
lookupCovariance x y (Covaried c) = HM.lookup (x,y) c <|> HM.lookup (y,x) c

{-# INLINE covariances #-}
covariances :: (Foldable f, Extract a) => f a -> Covaried
covariances = Foldable.foldl' (flip covaried) (Covaried mempty)

{-# INLINE covaried #-}
covaried :: Extract a => a -> Covaried -> Covaried
covaried = updateCovaried . flip (extract "") mempty
  where
    {-# INLINE updateCovaried #-}
    updateCovaried :: [(String,Double)] -> Covaried -> Covaried
    updateCovaried cvs hm = hm <> Covaried (HM.fromList (fmap analyze pairs))
      where
        analyze ((x,xd),(y,yd)) = ((x,y),covary fst snd (xd,yd) mempty)
        pairs = [(x,y) | (x:ys) <- tails cvs, y <- ys]

instance {-# OVERLAPPING #-} (Extract a,Extract b) => Extract (a,b)
instance {-# OVERLAPPING #-} (Extract a,Extract b,Extract c) => Extract (a,b,c)
instance {-# OVERLAPPING #-} (Extract a,Extract b,Extract c,Extract d) => Extract (a,b,c,d)
instance {-# OVERLAPPING #-} (Extract a,Extract b,Extract c,Extract d,Extract e) => Extract (a,b,c,d,e)
instance {-# OVERLAPPING #-} (Extract a,Extract b,Extract c,Extract d,Extract e,Extract f) => Extract (a,b,c,d,e,f)
instance {-# OVERLAPPING #-} (Extract a,Extract b,Extract c,Extract d,Extract e,Extract f,Extract g) => Extract (a,b,c,d,e,f,g)

instance {-# OVERLAPPING #-} (Extract a,Extract b) => Extract (Either a b)

instance {-# OVERLAPPING #-} (Extract (f a), Extract (g a)) => Extract (Sum f g a)
instance {-# OVERLAPPING #-} (Extract (f a), Extract (g a)) => Extract (Product f g a)
instance {-# OVERLAPPING #-} (Extract a) => Extract (Const a b)
instance {-# OVERLAPPING #-} (Extract a) => Extract (Identity a)
instance {-# OVERLAPPING #-} (Extract (f ( g a))) => Extract (Compose f g a)

gExtractReal :: (Real a) => String -> a -> [(String,Double)] -> [(String,Double)]
gExtractReal nm a xs = (nm,realToFrac a) : xs

class Extract a where
  extract :: String -> a -> [(String,Double)] -> [(String,Double)]
  default extract :: (Generic a, GExtract (Rep a)) => String -> a -> [(String,Double)] -> [(String,Double)]
  extract nm = gExtract nm . from

instance {-# OVERLAPPABLE #-} Extract a where
  extract _ _ = id

instance {-# OVERLAPPING #-} Extract Double where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Float where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Int where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Int8 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Int16 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Int32 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Int64 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Integer where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Natural where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Word where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Word8 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Word16 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Word32 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} Extract Word64 where
  extract = gExtractReal

instance {-# OVERLAPPING #-} (Extract a) => Extract (TxtTrie a) where
  extract nm a m = extract nm (Trie.toList a) m

instance {-# OVERLAPPING #-} (Extract a) => Extract (Map String a) where
  extract nm a m = extract nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Extract a) => Extract (Map Txt a) where
  extract nm a m = extract nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Extract a) => Extract (HashMap String a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in extract n v m
        )
        xs
        (HM.toList a)

instance {-# OVERLAPPING #-} (Extract a) => Extract (HashMap Txt a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ fromTxt k
          in extract n v m
        )
        xs
        (HM.toList a)

instance {-# OVERLAPPING #-} (Extract a, Foldable f) => Extract (f (String,a)) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in extract n v m
        )
        xs
        a

instance {-# OVERLAPPING #-} (Extract a, Foldable f) => Extract (f (Txt,a)) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ fromTxt k
          in extract n v m
        )
        xs
        a

instance {-# OVERLAPPING #-} (Extract a, V.Vector v a) => Extract (v a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      V.ifoldl' (\m i v ->
        let n = x ++ show i
        in extract n v m
      )
      xs
      a

class GExtract a where
  gExtract :: String -> a x -> [(String,Double)] -> [(String,Double)]

instance ( Datatype d
         , GExtract a
         ) => GExtract (D1 d a) where
  gExtract base (M1 d) xs =
    gExtract base d xs

instance ( Constructor c
         , GExtract a
         ) => GExtract (C1 c a) where
  gExtract base (M1 c) xs =
    gExtract base c xs

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GUnlabeledFieldExtract a
         ) => GExtract (S1 ('MetaSel 'Nothing u s l) a) where
  gExtract base m@(M1 s) xs =
    gUnlabeledFieldExtract base 1 s xs

instance {-# OVERLAPPABLE #-}
         ( Selector s
         , GExtract a
         ) => GExtract (S1 s a) where
  gExtract base m@(M1 s) xs =
    let sn = selName m
        x | sn == ""   = base
          | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gExtract x s xs

instance Extract a => GExtract (K1 r a) where
  gExtract base (K1 a) = extract base a

instance (GExtract a, GExtract b) => GExtract (a :+: b) where
  gExtract base (L1 a) = gExtract base a
  gExtract base (R1 b) = gExtract base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordExtract (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GExtract (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gExtract base ss xs = gRecordExtract base 1 ss xs

instance {-# OVERLAPPABLE #-}
         ( GExtract a
         , GExtract b
         ) => GExtract (a :*: b) where
  gExtract base (a :*: b) xs = gExtract base b (gExtract base a xs)

class GUnlabeledFieldExtract a where
  gUnlabeledFieldExtract :: String -> Int -> a x -> [(String,Double)] -> [(String,Double)]

instance {-# OVERLAPPING #-} (GExtract a) => GUnlabeledFieldExtract (S1 ('MetaSel 'Nothing u s l) a) where
  gUnlabeledFieldExtract base index m@(M1 s) xs =
    let sn = show index
        x | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gExtract x s xs

instance {-# OVERLAPPABLE #-} (GExtract (S1 s a)) => GUnlabeledFieldExtract (S1 s a) where
  gUnlabeledFieldExtract base _ = gExtract base

instance Extract a => GUnlabeledFieldExtract (K1 r a) where
  gUnlabeledFieldExtract base index (K1 a) xs =
    let x | base == "" = show index
          | otherwise  = base ++ "." ++ show index
    in extract x a xs

class GRecordExtract a where
  gRecordExtract :: String -> Int -> a x -> [(String,Double)] -> [(String,Double)]

instance {-# OVERLAPPABLE #-}
         ( GUnlabeledFieldExtract s
         ) => GRecordExtract s where
  gRecordExtract base index s xs = gUnlabeledFieldExtract base index s xs

instance {-# OVERLAPPING #-}
         ( Selector sa
         , GExtract a
         , GRecordExtract sb
         ) => GRecordExtract (S1 sa a :*: sb) where
  gRecordExtract base index (sa :*: sb) xs =
    let x | base == "" = show index
          | otherwise  = base ++ "." ++ show index
    in gRecordExtract base (index + 1) sb (gExtract x sa xs)
