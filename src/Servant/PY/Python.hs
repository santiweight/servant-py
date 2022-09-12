{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Servant.PY.Python where

import           Data.Data
import           Servant.Foreign
import Parcel

data Python

instance Parcel a => HasForeignType Python ParcelRepr a where
  typeFor _ _ _ = evalParcelM $ repr (Proxy @a)

instance Parcel a => HasForeignType Python (ParcelM ParcelRepr) a where
  typeFor _ _ _ = repr (Proxy @a)
