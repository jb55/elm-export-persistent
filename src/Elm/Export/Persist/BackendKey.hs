-- |
-- Module      :  Elm.Export.Persist.Ent
-- Copyright   :  (C) 2016-17 William Casarin
-- License     :  MIT
-- Maintainer  :  William Casarin <bill@casarin.me>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Orphan instances needed for SQL keys
--
-- This is usually required, but optionally exported in case you have your own
-- already

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elm.Export.Persist.BackendKey () where

import GHC.Generics
import Elm
import Database.Persist
import Database.Persist.Sql

deriving instance Generic (BackendKey SqlBackend)
deriving instance ElmType (BackendKey SqlBackend)
