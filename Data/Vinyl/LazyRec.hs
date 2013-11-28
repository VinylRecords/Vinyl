{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
module Data.Vinyl.LazyRec where

import           Data.Vinyl.Classes
import           Control.Applicative
import           Data.Vinyl.Field
import           Data.Vinyl.Rec

data family LazyRec (rs :: [*]) (f :: (* -> *)) :: *
data instance LazyRec '[] f = RNilL
data instance LazyRec ((sy ::: t) ': rs) f =  f t :&~ LazyRec rs f

infixr :&~

deriving instance Show (LazyRec '[] f)
deriving instance (Show (LazyRec rs f), Show (f t)) => Show (LazyRec ((sy ::: t) ': rs) f)    

instance Eq (LazyRec '[] f) where
  _ == _ = True
instance (Eq (g t), Eq (LazyRec fs g)) => Eq (LazyRec ((s ::: t) ': fs) g) where
  ~(x :&~ xs) == ~(y :&~ ys) = (x == y) && (xs == ys)
  
class ToRec rs f where
  toRec :: LazyRec rs f -> Rec rs f
instance ToRec '[] f where
  toRec RNilL = RNil
instance ToRec rs f => ToRec ((sy ::: t) ': rs) f where
  toRec (x :&~ xs) = x :& toRec xs
    
class FromRec rs f where
  fromRec :: Rec rs f -> LazyRec rs f
instance FromRec '[] f where
  fromRec RNil = RNilL
instance FromRec rs f => FromRec ((sy ::: t) ': rs) f where
  fromRec (x :& xs) = x :&~ (fromRec xs)

instance Dist (LazyRec '[]) where
  dist _ = pure RNilL
instance Dist (LazyRec rs) => Dist (LazyRec ((sy ::: t) ': rs)) where
  dist ~(x :&~ xs) = (:&~) <$> (pure <$> x) <*> dist xs

--
-- Generalized instances for Issue25 branch
--

-- instance Funct (LazyRec '[]) where
--   _  <<$>> _      = RNilL
-- instance Funct (LazyRec rs) => Funct (LazyRec ((sy ::: t) ': rs)) where
--   nat <<$>> ~((:&~) x xs) = (:&~) (nat x) (nat <<$>> xs)
  
-- instance Dist (LazyRec '[]) where
--   dist _ _      = pure RNilL -- should we pattern match on the RNilL?
-- instance Dist (LazyRec rs) => Dist (LazyRec ((sy ::: t) ': rs)) where
--   dist m ~((:&~) x xs) = (:&~) <$> (m x) <*> dist m xs
  
instance FoldRec (LazyRec '[] f) a where
  foldRec _ z _ = z -- should we pattern match on the RNilL?
instance FoldRec (LazyRec fs g) (g t) => FoldRec (LazyRec ((s ::: t) ': fs) g) (g t) where
  foldRec f z ~(x :&~ xs) = f x (foldRec f z xs)

instance Apply (~>) (LazyRec '[]) where
  _ <<*>> _ = RNilL -- again, pattern match?
instance Apply (~>) (LazyRec rs) => Apply (~>) (LazyRec ((sy ::: t) ': rs)) where
  ~(f :&~ fs) <<*>> ~(x :&~ xs) = runNT f x :&~ (fs <<*>> xs)
