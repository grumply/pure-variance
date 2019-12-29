module Pure.Variance (Variance,minimum_,maximum_,mean,mean2,count,stdDev,variance,vary,varies,Vary(..),Varied,getVariance,variances) where

import Pure.Data.JSON
import Pure.Data.Txt hiding (count)

import Data.Int
import Data.Word
import Data.Maybe
import GHC.Generics as G
import GHC.TypeLits

import Data.HashMap.Strict as HM

import qualified Data.Vector.Generic as V

data Variance
  = Variance
    { vCount   :: {-# UNPACK #-} !Double
    , vMean    :: {-# UNPACK #-} !Double
    , vMean2   :: {-# UNPACK #-} !Double
    , vMinimum :: {-# UNPACK #-} !Double
    , vMaximum :: {-# UNPACK #-} !Double
    } deriving (Eq,Ord,Generic,ToJSON,FromJSON,Show)

instance Monoid Variance where
  {-# INLINE mempty #-}
  mempty = Variance 0 0 0 0 0

instance Semigroup Variance where
  {-# INLINE (<>) #-}
  (<>) v1 v2
    | vCount v1 == 0 = v2
    | vCount v2 == 0 = v1
    | otherwise =
      let
        c1 = vCount v1
        c2 = vCount v2
        m1 = vMean v1
        m2 = vMean v2

        c  = c1 + c2

        m = (c1 * m1 + c2 * m2) / c

        ma = fromMaybe 0 (variance v1) * (c1 - 1)
        mb = fromMaybe 0 (variance v2) * (c2 - 1)
        d  = m1 - m2
        m2' = ma + mb + d ^^ 2 * c1 * c2 / c

        min_ = min (vMinimum v1) (vMinimum v2)
        max_ = max (vMaximum v1) (vMaximum v2)

      in
        Variance c m m2' min_ max_

count :: Variance -> Int
count = round . vCount

mean :: Variance -> Maybe Double
mean v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMean v)

mean2 :: Variance -> Maybe Double
mean2 v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMean2 v)

minimum_ :: Variance -> Maybe Double
minimum_ v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMinimum v)

maximum_ :: Variance -> Maybe Double
maximum_ v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMaximum v)

{-# INLINE vary #-}
vary :: Real a => a -> Variance -> Variance
vary (realToFrac -> a) Variance {..} =
  let count = vCount + 1
      delta = a - vMean
      mean = vMean + (delta / count)
      mean2 = vMean2 + delta * (a - mean)
      mx = max a vMaximum
      mn = if vCount == 0 then a else min a vMinimum
  in
    Variance count mean mean2 mn mx

{-# RULES
"vary a mempty" forall a. vary a mempty = let r = realToFrac a in Variance 1 r 0 r r
"flip vary mempty a" forall a. flip vary mempty a = let r = realToFrac a in Variance 1 r 0 r r
  #-}

{-# INLINE varies #-}
varies :: (Foldable f, Real b) => (a -> b) -> f a -> Variance
varies f = Prelude.foldr (vary . f) mempty

{-# INLINE variance #-}
variance :: Variance -> Maybe Double
variance Variance {..} = if vCount < 2 then Nothing else Just $ vMean2 / (vCount - 1)

{-# INLINE stdDev #-}
stdDev :: Variance -> Maybe Double
stdDev = fmap sqrt . variance

newtype Varied = Varied (HashMap String Variance)
 deriving (Show,Eq,Generic)

{-# INLINE getVariance #-}
getVariance :: String -> Varied -> Maybe Variance
getVariance s (Varied v) = HM.lookup s v

{-# INLINE variances #-}
variances :: (Vary a, Foldable f) => f a -> Varied
variances = Prelude.foldr (varied "") (Varied mempty)

class Vary a where
  varied :: String -> a -> Varied -> Varied
  default varied :: (Generic a, GVary (Rep a)) => String -> a -> Varied -> Varied
  varied _ a hm = gUpdateVariance "" (from a) hm

{-# INLINE gUpdateRealVariance #-}
gUpdateRealVariance :: (Real a) => String -> a -> Varied -> Varied
gUpdateRealVariance nm a (Varied hm) =
  Varied (HM.alter (Just . maybe (vary a mempty) (vary a)) nm hm)

instance {-# OVERLAPPABLE #-} Vary a where
  varied _ _ = id

instance {-# OVERLAPPING #-} Vary Double where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Float where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Integer where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} (Real a, Vary a) => Vary (HashMap String a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Real a, Foldable f, Vary a) => Vary (f (String,a)) where
  varied nm a (Varied hm) =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      Varied $
        Prelude.foldr
          (\(k,v) m ->
            let n = x ++ k
            in HM.alter (Just . maybe (vary v mempty) (vary v)) n m
          )
          hm
          a

instance {-# OVERLAPPING #-} (Real a, Vary a) => Vary (HashMap Txt a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Real a, Foldable f, Vary a) => Vary (f (Txt,a)) where
  varied nm a (Varied hm) =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      Varied $
        Prelude.foldr
          (\(k,v) m ->
            let n = x ++ fromTxt k
            in HM.alter (Just . maybe (vary v mempty) (vary v)) n m
          )
          hm
          a

instance {-# OVERLAPPING #-} (Real a, Vary a, V.Vector v a) => Vary (v a) where
  varied nm a (Varied hm) =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      Varied $
        V.ifoldr (\i v m ->
          let n = x ++ show i
          in HM.alter (Just . maybe (vary v mempty) (vary v)) n m
        )
        hm
        a

class GVary a where
  gUpdateVariance :: String -> a x -> Varied -> Varied

instance ( Datatype d
         , GVary a
         ) => GVary (D1 d a) where
  gUpdateVariance base m@(M1 d) hm =
    let dn = datatypeName m
    in gUpdateVariance (if Prelude.null base then dn else base ++ "." ++ dn) d hm

instance ( Constructor c
         , GVary a
         ) => GVary (C1 c a) where
  gUpdateVariance base m@(M1 c) hm =
    let cn = conName m
    in gUpdateVariance (base ++ "." ++ cn) c hm

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GUnlabeledFieldVary a
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateVariance base m@(M1 s) hm =
    gUpdateUnlabeledFieldVariance base 1 s hm

instance {-# OVERLAPPABLE #-}
         ( Selector s
         , GVary a
         ) => GVary (S1 s a)  where
  gUpdateVariance base m@(M1 s) hm =
    let sn = selName m
    in gUpdateVariance (base ++ "." ++ sn) s hm

instance (Vary a) => GVary (K1 r a) where
  gUpdateVariance base (K1 a) hm =
    varied base a hm

instance (GVary a, GVary b) => GVary (a :+: b) where
  gUpdateVariance base (L1 a) = gUpdateVariance base a
  gUpdateVariance base (R1 b) = gUpdateVariance base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordVary (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gUpdateVariance base ss hm = gUpdateRecordVariance base 0 ss hm

instance {-# OVERLAPPABLE #-}
         ( GVary a
         , GVary b
         ) => GVary (a :*: b) where
  gUpdateVariance base (a :*: b) hm = gUpdateVariance base b (gUpdateVariance base a hm)

class GUnlabeledFieldVary a where
  gUpdateUnlabeledFieldVariance :: String -> Int -> a x -> Varied -> Varied

instance (Selector s, GVary a) => GUnlabeledFieldVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateUnlabeledFieldVariance base index m@(M1 s) hm =
    let sn = show index
    in gUpdateVariance (base ++ "." ++ sn) s hm

instance (GVary (S1 s a)) => GUnlabeledFieldVary (S1 s a) where
  gUpdateUnlabeledFieldVariance base _ = gUpdateVariance base

instance (Vary a) => GUnlabeledFieldVary (K1 r a) where
  gUpdateUnlabeledFieldVariance base index (K1 a) hm =
    varied (base ++ "." ++ show index) a hm

class GRecordVary a where
  gUpdateRecordVariance :: String -> Int -> a x -> Varied -> Varied

instance {-# OVERLAPPABLE #-}
         ( GUnlabeledFieldVary s
         ) => GRecordVary s where
  gUpdateRecordVariance base index s hm = gUpdateUnlabeledFieldVariance base index s hm

instance {-# OVERLAPPING #-}
         ( Selector sa
         , GVary a
         , GRecordVary sb
         ) => GRecordVary (S1 sa a :*: sb) where
   gUpdateRecordVariance base index (sa :*: sb) hm =
     let nm = base ++ "." ++ show index
     in gUpdateRecordVariance base (index + 1) sb (gUpdateVariance nm sa hm)