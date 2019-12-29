module Pure.Covariance (Covariance,covary,covaries,covariance,count,meanx,meany,meanx2,meany2,c,variance_x,variance_y,stdDev_x,stdDev_y,correlation,Extract(..),Covaried,lookupCovariance,covaried,covarieds) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,fromTxt)
import Pure.Variance (Vary(),varies,variance,stdDev)

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
"covary f g a mempty == Convariance 1 (realToFrac (f a)) (realToFrac (g a)) 0 0 0" forall f g a. covary f g a (Covariance 0 0 0 0 0 0) = Covariance 1 (realToFrac (f a)) (realToFrac (g a)) 0 0 0
"variance_x (covaries f g as) == variance (varies f as)" forall f g as. variance_x (covaries f g as) = variance (varies f as)
"variance_y (covaries f g as) == variance (varies g as)" forall f g as. variance_y (covaries f g as) = variance (varies g as)
"stdDev_x (covaries f g as) == stdDev (varies f as)" forall f g as. stdDev_x (covaries f g as) = stdDev (varies f as)
"stdDev_y (covaries f g as) == stdDev (varies g as)" forall f g as. stdDev_y (covaries f g as) = stdDev (varies g as)
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

{-# INLINE [1] covariance #-}
covariance :: Covariance -> Maybe Double
covariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / (cCount - 1)

{-# INLINE [1] variance_x #-}
variance_x :: Covariance -> Maybe Double
variance_x Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / (cCount - 1)

{-# INLINE [1] variance_y #-}
variance_y :: Covariance -> Maybe Double
variance_y Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / (cCount - 1)

{-# INLINE [1] stdDev_x #-}
stdDev_x :: Covariance -> Maybe Double
stdDev_x = fmap sqrt . variance_x

{-# INLINE [1] stdDev_y #-}
stdDev_y :: Covariance -> Maybe Double
stdDev_y = fmap sqrt . variance_y

-- linear correlation; Pearson
{-# INLINE correlation #-}
correlation :: Covariance -> Maybe Double
correlation c = do
  cov <- covariance c
  sdx <- stdDev_x c
  sdy <- stdDev_y c
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
lookupCovariance x y (Covaried c) = HM.lookup (x,y) c

{-# INLINE covarieds #-}
covarieds :: (Foldable f, Extract a) => f a -> Covaried
covarieds = Foldable.foldl' (flip covaried) (Covaried mempty)

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

instance {-# OVERLAPPING #-} (Extract a) => Extract (HashMap String a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in extract n a m
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
          in extract n a m
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
          in extract n a m
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
          in extract n a m
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
        in extract n a m
      )
      xs
      a

class GExtract a where
  gExtract :: String -> a x -> [(String,Double)] -> [(String,Double)]

instance ( Datatype d
         , GExtract a
         ) => GExtract (D1 d a) where
  gExtract base m@(M1 d) xs =
    let dn = datatypeName m
    in gExtract (if Prelude.null base then dn else base ++ "." ++ dn) d xs

instance ( Constructor c
         , GExtract a
         ) => GExtract (C1 c a) where
  gExtract base m@(M1 c) xs =
    let cn = conName m
    in gExtract (base ++ "." ++ cn) c xs

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
    in gExtract (base ++ "." ++ sn) s xs

instance Extract a => GExtract (K1 r a) where
  gExtract base (K1 a) = extract base a

instance (GExtract a, GExtract b) => GExtract (a :+: b) where
  gExtract base (L1 a) = gExtract base a
  gExtract base (R1 b) = gExtract base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordExtract (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GExtract (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gExtract base ss xs = gRecordExtract base 0 ss xs

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
    in gExtract (base ++ "." ++ sn) s xs

instance {-# OVERLAPPABLE #-} (GExtract (S1 s a)) => GUnlabeledFieldExtract (S1 s a) where
  gUnlabeledFieldExtract base _ = gExtract base

instance Extract a => GUnlabeledFieldExtract (K1 r a) where
  gUnlabeledFieldExtract base index (K1 a) xs =
    extract (base ++ "." ++ show index) a xs

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
    let nm = base ++ "." ++ show index
    in gRecordExtract base (index + 1) sb (gExtract nm sa xs)
