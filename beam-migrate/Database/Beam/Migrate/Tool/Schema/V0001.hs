{-# LANGUAGE DeriveGeneric #-}
module Database.Beam.Migrate.Tool.Schema.V0001 where

import Database.Beam
import Database.Beam.Migrate.SQL
import Database.Beam.Migrate.Types

import Data.Text (Text)
import Data.Time (LocalTime)

import GHC.Generics

-- * Migration schema number table

data MigrationSchemaVersionT f
  = MigrationSchemaVersionT
  { migrationSchemaVersion :: Columnar f Int
  } deriving Generic
instance Beamable MigrationSchemaVersionT

instance Table MigrationSchemaVersionT where
  data PrimaryKey MigrationSchemaVersionT f
    = MigrationSchemaVersionId (Columnar f Int) deriving Generic
  primaryKey = MigrationSchemaVersionId . migrationSchemaVersion
instance Beamable (PrimaryKey MigrationSchemaVersionT)  

-- * Migration table

data MigrationT f
  = MigrationT
  { migrationNumber :: Columnar f Int
  , migrationName   :: Columnar f Text
  , migrationRanAt  :: Columnar f LocalTime
  } deriving Generic
instance Beamable MigrationT
type MigrationTable = MigrationT Identity
deriving instance Show MigrationTable; deriving instance Eq MigrationTable

instance Table MigrationT where
  data PrimaryKey MigrationT f = MigrationId (Columnar f Int) deriving Generic
  primaryKey = MigrationId . migrationNumber
instance Beamable (PrimaryKey MigrationT)

data MigrationDb f
  = MigrationDb
  { migrationDbVersions :: f MigrationSchemaVersionT
  , migrationDbMigratiosn :: f MigrationT
  } deriving Generic
instance Database MigrationDb

migration :: IsSql92DdlCommandSyntax syntax =>
             Migration syntax (DatabaseSettings MigrationDb)
migration = MigrationDb
  <$> createTable "beam_migration_version"
        (MigrationSchemaVersionT (field "version" int))
  <*> createTable "beam_migration"
        (MigrationT (field "number" int) (field "name" (varchar Nothing)) (field "ran_at" timestamptz))
