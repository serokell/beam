{-# LANGUAGE CPP                  #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Schema.ForeignKeys
    ( SqlForeignKey (..)
    , GAutoTableForeignKeys (..)
    , AutoTableForeignKeys (..)
    , GAutoDbForeignKeys (..)
    , defaultTableForeignKeys
    , defaultDbForeignKeys
    ) where

import Data.DList (DList)
import qualified Data.DList as DL
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import Data.Text (Text)

import GHC.Exts (toList)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)

import Database.Beam.Schema.Lookup
import Database.Beam.Schema.Tables

-- | Single foreign key settings.
data SqlForeignKey = SqlForeignKey
    { siTable          :: !Text
    , siFields         :: !(NonEmpty Text)
    , siReferredTable  :: !Text
    , siReferredFields :: !(NonEmpty Text)
    } deriving (Show, Eq, Ord)

-- * Automatic foreign keys derivation

buildSqlForeignKey
    :: (Beamable (PrimaryKey tbl'), Table tbl')
    => Text
    -> [Text]
    -> Text
    -> TableSettings tbl'
    -> DList SqlForeignKey
buildSqlForeignKey tblNm fieldNames referredTblNm referredTblSettings = do
    let referredFields = primaryKey referredTblSettings
        referredFieldNames = allBeamValues unFieldName referredFields
    -- we allow ourselves not to arise a compile-time error when the primary key is empty,
    -- the user will have even larger problems in such case anyway.
    maybe DL.empty DL.singleton $ do
        let siTable = tblNm
        siFields <- nonEmpty fieldNames
        let siReferredTable = referredTblNm
        siReferredFields <- nonEmpty referredFieldNames
        return SqlForeignKey{..}
  where
    unFieldName (Columnar' (TableField fieldNm)) = fieldNm

-- | Generic helper for 'AutoTableIndices'.
class GAutoTableForeignKeys be db x where
    autoTableForeignKeys' :: x -> Text -> DatabaseSettings be db -> DList SqlForeignKey
instance GAutoTableForeignKeys be db (x p) => GAutoTableForeignKeys be db (M1 i f x p) where
    autoTableForeignKeys' (M1 x) = autoTableForeignKeys' x
instance (GAutoTableForeignKeys be db (x p), GAutoTableForeignKeys be db (y p)) =>
          GAutoTableForeignKeys be db ((x :*: y) p) where
    autoTableForeignKeys' (x :*: y) = autoTableForeignKeys' x <> autoTableForeignKeys' y

instance {-# OVERLAPPABLE #-} GAutoTableForeignKeys be db (Rec0 x p) where
    autoTableForeignKeys' _ = mempty

instance GetDbEntity TableEntity tbl be db =>
         GAutoTableForeignKeys be db (Rec0 (PrimaryKey tbl (TableField tbl')) p) where
    autoTableForeignKeys' (K1 key) referringTblNm dbSettings = do
        DatabaseEntity (DatabaseTable referredTblNm referredTblSettings) <-
            pure $ getDbEntity (Proxy @TableEntity) (Proxy @tbl) dbSettings
        let fieldNames = allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) key
        buildSqlForeignKey referringTblNm fieldNames referredTblNm referredTblSettings

instance GetDbEntity TableEntity tbl be db =>
         GAutoTableForeignKeys be db (Rec0 (PrimaryKey tbl (Nullable (TableField tbl'))) p) where
    autoTableForeignKeys' (K1 key) referringTblNm dbSettings = do
        DatabaseEntity (DatabaseTable referredTblNm referredTblSettings) <-
            pure $ getDbEntity (Proxy @TableEntity) (Proxy @tbl) dbSettings
        let fieldNames = allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) key
        buildSqlForeignKey referringTblNm fieldNames referredTblNm referredTblSettings

-- | Traverses the table and for every field which is some 'PrimaryKey' makes a
-- corresponding SQL index in the referred table.
-- If a foreign key cannot be resolved within the given database, compile error arises.
class AutoTableForeignKeys be db entity where
    autoTableForeignKeys :: entity -> DatabaseSettings be db -> DList SqlForeignKey
instance {-# OVERLAPPABLE #-}
         (be ~ be', db ~ db') =>
         AutoTableForeignKeys be db (DatabaseEntity be' db' (entity (table :: (* -> *) -> *))) where
    autoTableForeignKeys _ = mempty
instance (Generic (TableSettings table),
          GAutoTableForeignKeys be db (Rep (TableSettings table) ()),
          be ~ be', db ~ db'
         ) =>
         AutoTableForeignKeys be db (DatabaseEntity be' db' (TableEntity table)) where
    autoTableForeignKeys (DatabaseEntity (DatabaseTable tblName tblSettings)) =
        autoTableForeignKeys' (from @_ @() tblSettings) tblName

-- | Automatically creates indices for every 'PrimaryKey' embedded into the given table.
-- Resulting indices appear exactly in the order in which 'PrimaryKey's are encountered in
-- the database. Indices may repeat (TODO: note that it is okay).
defaultTableForeignKeys
    :: (Generic (TableSettings table),
        GAutoTableForeignKeys be db (Rep (TableSettings table) ()))
    => DatabaseSettings be db
    -> DatabaseEntity be db (TableEntity table)
    -> [SqlForeignKey]
defaultTableForeignKeys dbSettings dbTable =
    toList $ autoTableForeignKeys dbTable dbSettings

-- | Traverses all tables in database and builds indices for all encountered 'PrimaryKey's.
class GAutoDbForeignKeys be db x where
    autoDbForeignKeys' :: x -> DatabaseSettings be db -> DList SqlForeignKey
instance GAutoDbForeignKeys be db (x p) => GAutoDbForeignKeys be db (M1 i f x p) where
    autoDbForeignKeys' (M1 x) = autoDbForeignKeys' x
instance (GAutoDbForeignKeys be db (x p), GAutoDbForeignKeys be db (y p)) =>
         GAutoDbForeignKeys be db ((x :*: y) p) where
    autoDbForeignKeys' (x :*: y) = autoDbForeignKeys' x <> autoDbForeignKeys' y
instance AutoTableForeignKeys be db x => GAutoDbForeignKeys be db (Rec0 x p) where
    autoDbForeignKeys' (K1 x) dbSettings = autoTableForeignKeys x dbSettings

-- | Automatically creates indices for every 'PrimaryKey' embedded into a table of the
-- given schema.
-- Resulting indices appear exactly in the order in which 'PrimaryKey's are encountered in
-- the database. Indices may repeat (TODO: note that it is okay).
defaultDbForeignKeys
    :: forall be db.
       (Generic (DatabaseSettings be db),
        GAutoDbForeignKeys be db (Rep (DatabaseSettings be db) ()))
    => DatabaseSettings be db -> [SqlForeignKey]
defaultDbForeignKeys db =
    toList $ autoDbForeignKeys' (from @_ @() db) db
