-- |
-- Module      :  Elm.Export.Persist.Ent
-- Copyright   :  (C) 2016-17 William Casarin
-- License     :  MIT
-- Maintainer  :  William Casarin <bill@casarin.me>
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Elm.Export.Persist.Ent
    ( Ent(..)
    , EntId
    ) where

import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import Elm
import Data.Proxy
import Data.Text
import Data.Scientific
import GHC.TypeLits
import GHC.Generics

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

-- | 'Entity' wrapper that adds `ToJSON`, `FromJSON`, and `ElmType` instances
--
-- The first type parameter 'field' is a symbol used for the key name
--
-- >>> toElmTypeSource (Proxy :: Proxy (Ent "userId" User))
-- "type alias User = { userName : String, userId : Int }"
newtype Ent (field :: Symbol) a = Ent (Entity a)
  deriving (Generic)

-- | 'Ent' alias, using "id" as the key
--
-- >>> toElmTypeSource (Proxy :: Proxy (EntId User))
-- "type alias User = { userName : String, id : Int }"
type EntId a = Ent "id" a

elmIdField :: Text -> ElmValue
elmIdField keyfield =
  ElmField keyfield (ElmPrimitiveRef EInt)

addIdToVals :: String -> ElmValue -> ElmValue
addIdToVals keyname ev =
  case ev of
    ef@(ElmField{}) ->
      Values ef (elmIdField (T.pack keyname))
    Values v1 rest -> Values v1 (addIdToVals keyname rest)
    _ -> ev


instance (KnownSymbol field, ElmType a) => ElmType (Ent field a) where
  toElmType _ =
    case toElmType (Proxy :: Proxy a) of
      ElmDatatype name (RecordConstructor x (Values v vals)) ->
        ElmDatatype name (RecordConstructor x
                            (Values v (addIdToVals keyname vals)))
      ElmDatatype name (RecordConstructor x f@(ElmField _ _)) ->
        ElmDatatype name (RecordConstructor x
                            (Values f $ elmIdField (T.pack keyname)))
      x -> x
    where
      keyname :: String
      keyname = symbolVal (Proxy :: Proxy field)

instance (KnownSymbol field, ToJSON a) => ToJSON (Ent field a) where
  toJSON (Ent (Entity k val)) =
    case toJSON val of
      Object hmap -> Object (Map.insert keyname (toJSON k) hmap)
      x           -> x
    where
      keyname :: Text
      keyname = T.pack $ symbolVal (Proxy :: Proxy field)

valToKey :: ToBackendKey SqlBackend record => Value -> Maybe (Key record)
valToKey (Number n) = toSqlKey <$> toBoundedInteger n
valToKey _          = Nothing

instance ( ToBackendKey SqlBackend a
         , PersistEntity a
         , KnownSymbol field
         , FromJSON a) => FromJSON (Ent field a) where
  parseJSON obj@(Object o) =
    let
      keyname :: String
      keyname = symbolVal (Proxy :: Proxy field)
      mkey = Map.lookup (T.pack keyname) o
      keyParser = do key <- maybe (fail $ "Ent: no key found for field " ++ keyname)
                            pure mkey
                     maybe (fail "Ent: could not parse key as Int64")
                           pure (valToKey key)
    in
      Ent <$>
        (Entity <$> keyParser
                <*> parseJSON obj)
  parseJSON _ = fail "Ent: should be an Object"

