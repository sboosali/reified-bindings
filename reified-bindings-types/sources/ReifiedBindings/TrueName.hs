{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, ScopedTypeVariables #-}
{-|

-}
module ReifiedBindings.TrueName where
import ReifiedBindings.Extra

import System.Mem.StableName
import Data.Type.Coercion
import Unsafe.Coerce
import Data.Coerce


{- | "knowing one's true name gives you power over them". 

laws:

* Equality: if @('nf' ('is' f g) '==' Just _)@ then @('unsafeCoerce' f \`'eq1'\` 'unsafeCoerce' g)@ given @('NFData1' f, 'Eq1' f)@
(TODO strict Maybe? Just must force Refl/Coercion/etc)
* Uniqueness: 

types:

* @f a@ holds the "true name" of some @a@
* @equals@ witnesses (a notion of) type-equality

-}
class TrueName f equals | f -> equals where
 is :: f a -> f b -> Maybe (equals a b)

{-| a stable name is a pointer with some properties:

* "stability":
* "": 

Two expressions with the same @StableName@ have the same runtime representation.
However, an expression and its @newtype@ (e.g. @0@ and @Sum 0@) can agree on their runtime representation,
while disagreeing on their type. Thus, 

TODO verify

-}
-- instance TrueName StableName Coercion where
--  is :: forall a b. StableName a -> StableName b -> Maybe (Coercion a b)
--  x `is` y = if x `eqStableName` y
--   then Just (unsafeCoerce (Coercion :: (Coercible a b) => Coercion a b)) --TODO Is it safe to Create a dictionary? How do we even create the dictionary? Is there even a dictionary, Coercible has no methods. use Constraints package? 
--   else Nothing

