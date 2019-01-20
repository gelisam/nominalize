{-# LANGUAGE KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Generics.Unrep where

import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GHC.Generics.MetaCons.Known
import GHC.Generics.MetaData.Known
import GHC.Generics.MetaSel.Known


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
makeUnrep :: forall (rep :: * -> *)
           . ( KnownMetaData        rep
             , KnownMetaConsList    rep
             , KnownMetaSelListList rep
             )
          => Proxy rep -> Q [Dec]
makeUnrep _ = do
  let metaData :: MetaData
      metaData = metaDataVal (Proxy @rep)

      metaConsList :: [MetaCons]
      metaConsList = metaConsListVal (Proxy @rep)

      metaSelListList :: [[MetaSel]]
      metaSelListList = metaSelListListVal (Proxy @rep)

  typeName :: Name
           <- newName (metaDataDatatypeName metaData ++ "2")

  let typeParameters :: [TyVarBndr]
      typeParameters = []

      makeBang :: MetaSel -> Q Bang
      makeBang metaSel = pure $ Bang (metaSelSourceUnpackedness metaSel)
                                     (metaSelSourceStrictness   metaSel)

      makeBangType :: MetaSel -> Q BangType
      makeBangType metaSel = (,)
                         <$> makeBang metaSel
                         <*> pure (ConT typeName)

      makeCon :: MetaCons -> [MetaSel] -> Q Con
      makeCon metaCons metaSelList = NormalC
                                 <$> newName (metaConsName metaCons ++ "2")
                                 <*> traverse makeBangType metaSelList

  constructors :: [Con]
               <- sequence (zipWith makeCon metaConsList metaSelListList)

  let dec :: Dec
      dec = DataD [] typeName typeParameters Nothing constructors []

  pure [dec]
