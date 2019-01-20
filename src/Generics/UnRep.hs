{-# LANGUAGE FlexibleContexts, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Generics.UnRep where

import Data.Proxy
import qualified Language.Haskell.TH as TH

import Generics.UnRep.ToTH


makeUnRep :: forall (rep :: * -> *). ToTH TH.Dec rep
          => Proxy rep -> TH.Q [TH.Dec]
makeUnRep _ = pure [toTH (Proxy @rep)]
