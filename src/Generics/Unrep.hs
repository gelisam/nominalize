{-# LANGUAGE KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Generics.Unrep where

import Data.Proxy
import Language.Haskell.TH

import GHC.Generics.MetaData.Known


-- |
-- >>> :set -XDeriveGeneric
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> import GHC.Generics
-- >>> :{
-- data Tree = Leaf | Branch Tree Tree deriving Generic
-- makeUnrep (Proxy @(Rep Tree))
-- :}
--
-- >>> :info Tree2
-- data Tree2 = Leaf2 | Branch2 Tree2 Tree2
-- ...
makeUnrep :: forall (rep :: * -> *). KnownMetaData rep
          => Proxy rep -> Q [Dec]
makeUnrep _ = do
  let metaData :: MetaData
      metaData = metaDataVal (Proxy @rep)
  typeName <- newName (metaDataDatatypeName metaData ++ "2")
  ctorName1 <- newName "Leaf2"
  ctorName2 <- newName "Branch2"

  defaultBang <- bang noSourceUnpackedness noSourceStrictness

  let typeParameters :: [TyVarBndr]
      typeParameters = []

      ctor1 :: Con
      ctor1 = NormalC ctorName1 []

      ctor2 :: Con
      ctor2 = NormalC ctorName2 [ (defaultBang, ConT typeName)
                                , (defaultBang, ConT typeName)
                                ]

      constructors :: [Con]
      constructors = [ctor1, ctor2]

      dec :: Dec
      dec = DataD [] typeName typeParameters Nothing constructors []

  pure [dec]
