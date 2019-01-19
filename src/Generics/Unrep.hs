{-# LANGUAGE KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Generics.Unrep where

import Data.Proxy
import Language.Haskell.TH

import GHC.Generics.MetaCons.Known
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
-- data Tree2 = Leaf2 Tree2 | Branch2 Tree2
-- ...
makeUnrep :: forall (rep :: * -> *)
           . ( KnownMetaData     rep
             , KnownMetaConsList rep
             )
          => Proxy rep -> Q [Dec]
makeUnrep _ = do
  let metaData :: MetaData
      metaData = metaDataVal (Proxy @rep)

      metaConsList :: [MetaCons]
      metaConsList = metaConsListVal (Proxy @rep)

  typeName :: Name
           <- newName (metaDataDatatypeName metaData ++ "2")

  defaultBang :: Bang
              <- bang noSourceUnpackedness noSourceStrictness

  let typeParameters :: [TyVarBndr]
      typeParameters = []

      makeCon :: MetaCons -> Q Con
      makeCon metaCons = do
        name <- newName (metaConsName metaCons ++ "2")
        pure $ NormalC name [(defaultBang, ConT typeName)]

  constructors :: [Con]
               <- traverse makeCon metaConsList

  let dec :: Dec
      dec = DataD [] typeName typeParameters Nothing constructors []

  pure [dec]
