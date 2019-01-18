{-# LANGUAGE ScopedTypeVariables #-}
module Generics.Unrep where

import Language.Haskell.TH


-- |
-- >>> :set -XTemplateHaskell
-- >>> :{
-- data Tree = Leaf | Branch Tree Tree
-- makeUnrep
-- :}
--
-- >>> :info Tree2
-- data Tree2 = Leaf2 | Branch2 Tree2 Tree2
-- ...
makeUnrep :: Q [Dec]
makeUnrep = do
  typeName <- newName "Tree2"
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
