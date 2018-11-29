{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Database.Beam.Schema.Indices
    ( SqlTableIndex (..)
    , SqlIndex (..)
    , SqlTableIndexBuilder

    , FieldIndexBuilder (..)
    , IndexBuilder (..)
    , tableIndex
    , withTableIndex
    , createIndex

    , AutoEntityIndex (..)
    , GAutoDbIndices (..)
    , defaultDbIndices
    ) where

import Data.DList (DList)
import qualified Data.DList as DL
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, sort)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Exts (Constraint, fromList, toList)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)
import GHC.TypeLits

import Database.Beam.Schema.Tables

-- | Single index settings for the given table.
newtype SqlTableIndex = SqlTableIndex (NonEmpty Text)
    deriving (Show, Semigroup)

instance Eq SqlTableIndex where
    SqlTableIndex f1 == SqlTableIndex f2 = sort f1 == sort f2
instance Ord SqlTableIndex where
    SqlTableIndex f1 `compare` SqlTableIndex f2 = sort f1 `compare` sort f2

-- | Single index settings.
data SqlIndex = SqlIndex !Text !SqlTableIndex
    deriving (Show, Eq, Ord)

newtype SqlTableIndexBuilder table = SqlTableIndexBuilder (TableSettings table -> SqlTableIndex)

-- | Create indices for the given table.
--   In most cases you probably want it to accept a list of builders created with 'tableIndex'.
withTableIndex
    :: (Functor t)
    => DatabaseEntity be db (TableEntity table)
    -> t (SqlTableIndexBuilder table)
    -> t SqlIndex
withTableIndex (DatabaseEntity (DatabaseTable tblNm tblSettings)) fetchIndices =
    fmap (\(SqlTableIndexBuilder makeIndex) -> SqlIndex tblNm $ makeIndex tblSettings)
         fetchIndices

createIndex :: SqlIndex -> Text
createIndex (SqlIndex tblNm (SqlTableIndex (toList -> fields))) =
    "ALTER TABLE " <> tblNm <>
    " CREATE INDEX IF NOT EXISTS " <> ("idx_" <> tblNm <> "_" <> T.intercalate "_" fields) <>
    " ON " <> tblNm <> "(" <> T.intercalate ", " fields <> ");"

-- * Manual indices definition

type family GIsNotEmptyData (item :: Symbol) (rep :: * -> *) :: Constraint where
    GIsNotEmptyData item (D1 _d (C1 _c U1)) =
        TypeError ('Text item ':<>: 'Text " without fields is not allowed here")
    GIsNotEmptyData _ _ = ()

type IsNotEmptyData item x = GIsNotEmptyData item (Rep x)

class FieldIndexBuilder field where
    buildFieldIndex :: field -> SqlTableIndex
instance FieldIndexBuilder (TableField table a) where
    buildFieldIndex field = SqlTableIndex . (:| []) $ _fieldName field
instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (TableField table')) where
    buildFieldIndex =
        SqlTableIndex . fromList .
        allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm)

instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (Nullable (TableField table'))) where
    buildFieldIndex =
        SqlTableIndex . fromList .
        allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm)

class IndexBuilder table a where
    buildIndex :: TableSettings table -> a -> SqlTableIndex

-- | Field accessors are building blocks for indices.
instance (f ~ TableField table, FieldIndexBuilder field) =>
         IndexBuilder table (table f -> field) where
    buildIndex settings getter =
        buildFieldIndex $ getter settings

instance (IndexBuilder table a, IndexBuilder table b) =>
         IndexBuilder table (a, b) where
    buildIndex settings (a, b) = buildIndex settings a <> buildIndex settings b

instance (IndexBuilder table a, IndexBuilder table b, IndexBuilder table c) =>
         IndexBuilder table (a, b, c) where
    buildIndex settings (a, b, c) =
        buildIndex settings a <> buildIndex settings b <> buildIndex settings c

-- | Make a table index builder covering the specified fields.
--   Basic usage is to pass a table field accesor or a tuple of them to this function.
tableIndex :: IndexBuilder table a => a -> SqlTableIndexBuilder table
tableIndex = SqlTableIndexBuilder . flip buildIndex

-- * Automatic indices definition

class AutoEntityIndex be db tbl where
    autoEntityIndex :: DatabaseEntity be db tbl -> Maybe SqlIndex
-- Other types of entities are approaching, and we probably don't want to define
-- instances for all of them.
instance {-# OVERLAPPABLE #-}
         AutoEntityIndex be db entity where
    autoEntityIndex _ = Nothing
instance AutoEntityIndex be db (TableEntity table) where
    autoEntityIndex (DatabaseEntity (DatabaseTable tblName tblSettings)) =
        let pkFields = allBeamValues
                           (\(Columnar' (TableField fieldNm)) -> fieldNm)
                           (primaryKey tblSettings)
        in SqlIndex tblName . SqlTableIndex <$> nonEmpty pkFields

-- | Traverses all tables in database and builds indices for all encountered 'PrimaryKey's.
class GAutoDbIndices x where
    autoDbIndices' :: x -> DList SqlIndex
instance GAutoDbIndices (x p) => GAutoDbIndices (M1 i f x p) where
    autoDbIndices' (M1 x) = autoDbIndices' x
instance (GAutoDbIndices (x p), GAutoDbIndices (y p)) =>
         GAutoDbIndices ((x :*: y) p) where
    autoDbIndices' (x :*: y) = autoDbIndices' x <> autoDbIndices' y
instance AutoEntityIndex be db tbl =>
         GAutoDbIndices (Rec0 (DatabaseEntity be db tbl) p) where
    autoDbIndices' (K1 entity) = maybe DL.empty DL.singleton (autoEntityIndex entity)

-- | Automatically creates indices for every 'PrimaryKey' embedded into a table.
--   Resulting indices appear exactly in the order in which 'PrimaryKey's are encountered in
--   the database. Indices may repeat (TODO: note that it is okay).
defaultDbIndices
    :: forall be db.
       (Generic (DatabaseSettings be db), GAutoDbIndices (Rep (DatabaseSettings be db) ()))
    => DatabaseSettings be db -> [SqlIndex]
defaultDbIndices db =
    toList $ autoDbIndices' (from @_ @() db)
