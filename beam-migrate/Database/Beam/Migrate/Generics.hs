{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Support for creating checked databases from Haskell ADTs, using 'Generic's.
--
-- For more information, see
-- <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>

module Database.Beam.Migrate.Generics
 ( -- * Default checked database settings
   defaultMigratableDbSettings
 , withIndices

 -- * Extending the defaulting sytem
 , HasDefaultSqlDataType(..), HasDefaultSqlDataTypeConstraints(..)
 , Sql92HasDefaultDataType
 ) where

import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types
import Database.Beam.Migrate.Types
import Database.Beam.Schema.Indices
import Database.Beam.Schema.Tables

import Data.Functor.Identity
import Data.Proxy

import GHC.Generics

-- | Produce a checked database for the given Haskell database type
--
-- See <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>
-- for more information on the defaults.
defaultMigratableDbSettings
  :: forall syntax be db.
   ( Generic (CheckedDatabaseSettings be db)
   , GAutoMigratableDb syntax (Rep (CheckedDatabaseSettings be db)) )
  => CheckedDatabaseSettings be db
defaultMigratableDbSettings =
  to (defaultMigratableDbSettings' (Proxy @syntax) :: Rep (CheckedDatabaseSettings be db) ())

-- | Attach checks which require the given indices to the checked database.
--
-- Note that this function nicely composes with 'withDbModification' when used
-- in infix form:
--
-- @
-- defaultMigratableDbSettings
--     `withDbModification` dbModification{ ... }
--     `withIndices` dbIndices{ ... }
-- @
withIndices
    :: forall be db.
       Database be db
    => CheckedDatabaseSettings be db
    -> DatabaseIndices be db
    -> CheckedDatabaseSettings be db
withIndices checkedDbSettings indices =
    runIdentity $
    zipTables (Proxy @be)
        (\(CheckedDatabaseEntity dbSettings dbPredicates) indexEntity ->
          pure $ CheckedDatabaseEntity (addIndexChecks dbSettings indexEntity) dbPredicates
        )
        checkedDbSettings indices

withDefaultForeignKeys
    :: CheckedDatabaseSettings be db -> CheckedDatabaseSettings be db
withDefaultForeignKeys _ = undefined
