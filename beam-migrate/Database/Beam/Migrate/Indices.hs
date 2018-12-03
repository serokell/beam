module Database.Beam.Migrate.Indices
    ( withDbIndices

    ) where

import Data.Functor.Identity
import Data.Proxy

import Database.Beam.Migrate.Types
import Database.Beam.Schema.Indices
import Database.Beam.Schema.Tables

class AddIndicesToCheckedEntity be db tbl where
    addIndicesToCheckedEntity
        :: CheckedDatabaseEntity be db tbl
        -> EntityIndices be tbl
        -> CheckedDatabaseEntity be db tbl
instance {-# OVERLLAPPING #-}
         AddIndicesToCheckedEntity be db tbl where
    addIndicesToCheckedEntity = const
instance AddIndicesToCheckedEntity be db (TableEntity table) where

withDbIndices
    :: forall be db.
       Database be db
    => CheckedDatabaseSettings be db
    -> DatabaseIndices be db
    -> CheckedDatabaseSettings be db
withDbIndices dbSettings dbIndices =
    runIdentity $ zipTables (Proxy @be) (\a b -> pure $ addIndicesToCheckedEntity a b) dbSettings dbIndices
