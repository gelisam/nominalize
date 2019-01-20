{-# LANGUAGE FlexibleContexts, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Generics.Unrep where

import Data.Proxy
import qualified Language.Haskell.TH as TH

import Generics.Unrep.ToTH


makeUnrep :: forall (rep :: * -> *). ToTH TH.Dec rep
          => Proxy rep -> TH.Q [TH.Dec]
makeUnrep _ = do
  dec :: TH.Dec <- toTH (Proxy @rep)
  pure [dec]
