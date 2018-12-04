{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Database.Beam.Schema.Indices
    ( TableIndex (..)
    , Index (..)
    , TableIndexBuilder

    , FieldIndexBuilder (..)
    , IndexBuilder (..)
    , EntityIndices (..)
    , DatabaseIndices
    , mkIndex
    , withTableIndex
    , dbIndices
    , tableIndex
    ) where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..), sort)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Exts (Constraint, fromList, toList)
import GHC.Generics hiding (C, R)
import GHC.TypeLits

import Database.Beam.Schema.Tables

-- TODO: format imports as they were everywhere!

-- | Single index settings for the given table.
newtype TableIndex = TableIndex (NonEmpty Text)
    deriving (Show, Semigroup)

instance Eq TableIndex where
    TableIndex f1 == TableIndex f2 = sort f1 == sort f2
instance Ord TableIndex where
    TableIndex f1 `compare` TableIndex f2 = sort f1 `compare` sort f2

-- | Single index settings.
data Index = Index !Text !TableIndex
    deriving (Show, Eq, Ord)

newtype TableIndexBuilder table = TableIndexBuilder (TableSettings table -> TableIndex)

-- | Indices for a 'table'.
--
--   Essentially a wrapper over a set of indices.
--
--   TODO:
--   Usually you use the 'defaultDbSettings' function to generate an appropriate
--   naming convention for you, and then modify it with 'withDbModification' if
--   necessary. Under this scheme, the field can be renamed using the 'IsString'
--   instance for 'TableField', or the 'fieldNamed' function.
newtype EntityIndices be db entity = EntityIndices
    { _entityIndices :: [DatabaseEntity be db entity -> TableIndex]
    } deriving (Semigroup, Monoid)

type DatabaseIndices be db = db (EntityIndices be db)

-- | Return empty 'DatabaseIndices' (not counting indices created automatically like
--   primary key index). You can use it like
--
-- > dbIndices { tbl1 = tableIndex field1 <> tableIndex (field2, field3) }
dbIndices :: forall be db. Database be db => DatabaseIndices be db
dbIndices = runIdentity $ zipTables (Proxy @be) (\_ _ -> pure mempty) undefined undefined

withTableIndex
    :: (Functor t)
    => DatabaseEntity be db (TableEntity table)
    -> t (TableIndexBuilder table)
    -> t Index
withTableIndex (DatabaseEntity (DatabaseTable tblNm tblSettings)) fetchIndices =
    fmap (\(TableIndexBuilder makeIndex) -> Index tblNm $ makeIndex tblSettings)
         fetchIndices

-- TODO make part of relevant 'ActionProvider' / create dedicated syntax
createIndex :: Index -> Text
createIndex (Index tblNm (TableIndex (toList -> fields))) =
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
    buildFieldIndex :: field -> TableIndex
instance FieldIndexBuilder (TableField table a) where
    buildFieldIndex field = TableIndex . (:| []) $ _fieldName field
instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (TableField table')) where
    buildFieldIndex =
        TableIndex . fromList .
        allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm)

instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (Nullable (TableField table'))) where
    buildFieldIndex =
        TableIndex . fromList .
        allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm)

class IndexBuilder table a where
    buildIndex :: TableSettings table -> a -> TableIndex

-- | Field accessors are building blocks for indices.
instance (f ~ TableField table, table ~ table', FieldIndexBuilder field) =>
         IndexBuilder table' (table f -> field) where
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
mkIndex :: IndexBuilder table a => a -> TableIndexBuilder table
mkIndex = TableIndexBuilder . flip buildIndex

tableIndex
    :: IndexBuilder table a
    => a
    -> EntityIndices be db (TableEntity table)
tableIndex builder = EntityIndices $
    [\(DatabaseEntity (DatabaseTable _ tblSettings)) -> buildIndex tblSettings builder]
