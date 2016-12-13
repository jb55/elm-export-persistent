{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Elm.Export.Persist.BackendKey () where

import GHC.Generics
import Elm
import Database.Persist
import Database.Persist.Sql

deriving instance Generic (BackendKey SqlBackend)
deriving instance ElmType (BackendKey SqlBackend)