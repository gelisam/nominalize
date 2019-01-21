-- Create a target datatype which is identical to the source datatype except
-- the name of the type, its constructors, and its fields have an extra prime
-- at the end.
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Examples.Prime where

import GHC.Generics
import GHC.TypeLits

import Generics.UnRep
import qualified Examples.Fake as Fake


-- |
-- >>> :set -XDeriveGeneric
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> import Data.Proxy
-- >>> data Person = Person { name :: Fake.String, age :: Int }  deriving Generic
-- :{
-- data Foo = Foo  -- to interpret the next line as TemplateHaskell, not an expression
-- makeUnRep (Proxy @(Prime Person))
-- :}
--
-- Make sure the generated type is also using 'Fake.String', not 'Prelude.String'.
-- >>> let person' = Person' Fake.String 42

type family Prime a where
  Prime a
    = Prime'Data (Rep a)

type family Prime'Data rep where
  Prime'Data (D1 (MetaData datatypeName moduleName packageName isNewtype) rep)
    = D1 (MetaData (Prime'Name datatypeName) moduleName packageName isNewtype)
         (Prime'Cons rep)

type family Prime'Cons rep where
  Prime'Cons V1
    = V1
  Prime'Cons (C1 (MetaCons name fixity isRecord) rep)
    = C1 (MetaCons (Prime'Name name) fixity isRecord)
         (Prime'Sel rep)
  Prime'Cons (rep1 :+: rep2)
    = Prime'Cons rep1 :+: Prime'Cons rep2

type family Prime'Sel rep where
  Prime'Sel U1
    = U1
  Prime'Sel (S1 (MetaSel maybeName sourceUnpackedness sourceStrictness decidedStrictness) rep)
    = S1 (MetaSel (Prime'MaybeName maybeName) sourceUnpackedness sourceStrictness decidedStrictness)
         rep
  Prime'Sel (rep1 :*: rep2)
    = Prime'Sel rep1 :*: Prime'Sel rep2

type family Prime'MaybeName maybeName where
  Prime'MaybeName 'Nothing
    = 'Nothing
  Prime'MaybeName ('Just name)
    = 'Just (Prime'Name name)

type family Prime'Name name where
  Prime'Name name = AppendSymbol name "'"
