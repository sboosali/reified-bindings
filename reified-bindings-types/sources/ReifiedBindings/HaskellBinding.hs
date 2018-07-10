{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, GADTs #-}
{-|

-}
module ReifiedBindings.HaskellBinding where
import ReifiedBindings.Extra
import ReifiedBindings.TrueName

import System.Mem.StableName
import Data.Type.Equality
import Unsafe.Coerce
import Language.Haskell.TH as TH
import Control.Exception (evaluate)
-- import Data.Function
-- import GHC.Generics (Generic1)
import Control.Monad.IO.Class

--------------------------------------------------------------------------------

{-| A Haskell binding has a left-hand-side and the  right-hand-side.
a 'HaskellBinding' is reified/unerased representation
of a declaration in the source code.

Declarations are "static".

@
HaskellBinding ≡ DSum HaskellName Identity
@

Can be unsafely constructed by quoting:

@
{-# LANGUAGE TemplateHaskell #-}
s = 'unsafeHaskellBinding' \'s "a string"   -- inferred :: 'HaskellBinding' String
@

Can be safely (though conveniently, for now) constructed with:

* a preprocessor: 
* a @TemplateHaskell@ @QuasiQuoter@: 

-}
data HaskellBinding a = HaskellBinding
 { _hsLHS :: HaskellName a
 , _hsRHS :: a
 } deriving (Show,Eq,Ord,Generic)

{-| Haskell expressions can be identifiers (i.e. 'hsName' is @Just _@)
or anonymous (i.e. 'hsName' is @Nothing@).

Expressions are "dynamic".

see 'StableName'?

is "dynamic" versus 'HaskellBinding' being "static".
i.e. new 'HaskellNames' can't be built at runtime
(except at "the previous stage's runtime" via @TemplateHaskell@ macros);
so .

-}
data HaskellExpression a = HaskellExpression
-- { hsName  :: Maybe TH.Name
 { _hsPointer    :: StableName a
 , _hsExpression :: a
 } deriving (Eq,Generic) -- TODO ,Ord

{-| A Template Haskell name (on the value-level), with the type (on the type-level)
of the identifier it names.

Can be unsafely constructed by quoting:

@
{-# LANGUAGE TemplateHaskell #-}
onomatopoeia = 'unsafeHaskellName' \'onomatopoeia :: 'HaskellName' 'TH.Name'
@

Can be safely (though conveniently, for now) constructed with:

* a preprocessor: 
* a @TemplateHaskell@ @QuasiQuoter@: 


-}
data HaskellName a = HaskellName
 { getHaskellName :: TH.Name
 } deriving (Show,Eq,Ord,Generic)

--------------------------------------------------------------------------------

{-|

-}
instance TrueName HaskellName (:~:) where
 x `is` y = if x `eqHaskellName` y
  then Just (unsafeCoerce Refl)
  else Nothing

{-| 

TODO safe with macro as only constructor

-}
instance TrueName HaskellBinding (:~:) where
 is x y = is (x&_hsLHS) (y&_hsLHS)
-- is `on` _hsLHS

--------------------------------------------------------------------------------

{-|

-}
unsafeHaskellName :: Name -> HaskellName a
unsafeHaskellName = HaskellName
{-# WARNING unsafeHaskellName "" #-}

{-|

-}
unsafeHaskellBinding :: Name -> a -> HaskellBinding a
unsafeHaskellBinding l r = HaskellBinding (unsafeHaskellName l) r
{-# WARNING unsafeHaskellBinding "" #-}

{-|

-}
eqHaskellName :: HaskellName a -> HaskellName b -> Bool
eqHaskellName x y = (x&getHaskellName) == (y&getHaskellName)
-- (==) `on` getHaskellName

{-|

-}
eqHaskellBinding :: (Eq a, Eq b) => HaskellBinding a -> HaskellBinding b -> Bool
eqHaskellBinding x y = case (x&_hsLHS) `is` (y&_hsLHS) of
  Nothing   -> False
  Just Refl -> x == y

{-|

NOTE forces the @a@.

-}
newHaskellExpression :: (MonadIO m) => a -> m (HaskellExpression a)
newHaskellExpression a = liftIO $ do
  _hsExpression <- evaluate a
  _hsPointer <- makeStableName _hsExpression
  return HaskellExpression{..}

--------------------------------------------------------------------------------

{-| A lifted 'HaskellBinding'.

Equivalent to:

@
type HaskellBinding1 = 'HIdentity' 'HaskellBinding'
@

-}
data HaskellBinding1 f a = HaskellBinding1
 { _hsLHS1 :: HaskellName (f a)
 , _hsRHS1 :: f a
 } deriving (Show,Eq,Ord,Generic)

-- TODO (Show1,Eq1,Ord1,Generic1)
-- Can't be derived
-- Data.Functor.Classes

--old
-- Generic1
-- No instance for (Functor HaskellName)


{-| A lifted 'HaskellExpression'.

Equivalent to:

@
type HaskellExpression1 = 'HIdentity' 'HaskellExpression'
@

-}
data HaskellExpression1 f a = HaskellExpression1
 { _hsPointer1    :: StableName (f a)
 , _hsExpression1 :: f a
 } deriving (Eq,Generic) -- TODO ,Ord

--------------------------------------------------------------------------------

instance TrueName (HaskellBinding1 f) (:~:) where
 is x y = case is (x&_hsLHS1) (y&_hsLHS1) of
   Nothing -> Nothing
   Just Refl -> Just Refl -- i.e. { (f a ~ f b) } proves { (a ~ b) }, as type constructors (like { f }) are injective
-- is `on` hsLHS

-- instance TrueName HaskellBinding1 (::~::) where
--  is x y = is (x&_hsLHS1) (y&_hsLHS1)
-- -- is `on` hsLHS

--------------------------------------------------------------------------------

{-|

-}
unsafeHaskellBinding1 :: Name -> f a -> HaskellBinding1 f a
unsafeHaskellBinding1 l r = HaskellBinding1 (unsafeHaskellName l) r
{-# WARNING unsafeHaskellBinding1 "" #-}

-- {-|

-- -}
-- eqHaskellBinding1 :: (Eq1 f) => HaskellBinding1 f a -> HaskellBinding1 f b -> Bool
-- -- eqHaskellBinding1 :: (Eq (f a), Eq (f b)) => HaskellBinding1 f a -> HaskellBinding1 f b -> Bool
-- eqHaskellBinding1 x y = case (x&_hsLHS) `is` (y&_hsLHS) of
--   Nothing   -> False
--   Just Refl -> x == y

{-|

NOTE forces the @a@.

-}
newHaskellExpression1 :: (MonadIO m) => f a -> m (HaskellExpression1 f a)
newHaskellExpression1 a = liftIO $ do
  _hsExpression1 <- evaluate a
  _hsPointer1 <- makeStableName _hsExpression1
  return HaskellExpression1{..}

--------------------------------------------------------------------------------
