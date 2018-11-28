{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Database.Beam.Schema.Indices
    ( SqlTableIndex (..)
    , SqlIndex (..)
    , FieldIndexBuilder (..)
    , IndexBuilder (..)
    , tableIndex
    , withTableIndex
    , createIndex

    , GAutoTableIndices (..)
    , AutoTableIndices (..)
    , GAutoDbIndices (..)
    , defaultDbIndices
    ) where

import Data.DList (DList)
import qualified Data.DList as DL
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Exts (Constraint, fromList, toList)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)
import GHC.TypeLits


import Database.Beam.Schema.Lookup
import Database.Beam.Schema.Tables

-- | Single index settings for the given table.
newtype SqlTableIndex = SqlTableIndex (NonEmpty Text)
    deriving (Show, Eq, Ord, Semigroup)

-- | Single index settings.
data SqlIndex = SqlIndex Text SqlTableIndex
    deriving (Show, Eq, Ord)

withTableIndex
    :: (Functor t)
    => DatabaseEntity be db (TableEntity table)
    -> t (TableSettings table -> SqlTableIndex)
    -> t SqlIndex
withTableIndex (DatabaseEntity (DatabaseTable tblNm tblSettings)) fetchIndices =
    fmap (\makeIndex -> SqlIndex tblNm $ makeIndex tblSettings) fetchIndices

createIndex :: SqlIndex -> Text
createIndex (SqlIndex tblNm (SqlTableIndex (toList -> fields))) =
    "ALTER TABLE " <> tblNm <>
    " CREATE INDEX IF NOT EXISTS " <> ("idx_" <> tblNm <> "_" <> T.intercalate "_" fields) <>
    " ON " <> tblNm <> "(" <> T.intercalate ", " fields <> ");"

-------------
-- Example --
-------------

data CourseRowT f = CourseRow
    { crId   :: C f Int
    , crDesc :: C f Text
    } deriving (Generic)

data SubjectRowT f = SubjectRow
    { srId     :: C f Int
    , srDesc   :: C f Text
    , srCourse :: PrimaryKey CourseRowT f
    } deriving (Generic)

data EducatorSchema f = EducatorSchema
    { esCourses  :: f (TableEntity CourseRowT)
    , esSubjects :: f (TableEntity SubjectRowT)
    } deriving (Generic)

instance Table CourseRowT where
    newtype PrimaryKey CourseRowT f = CourseRowId (C f Int)
        deriving (Generic)
    primaryKey = CourseRowId . crId

instance Table SubjectRowT where
    newtype PrimaryKey SubjectRowT f = SubjectRowId (C f Int)
        deriving (Generic)
    primaryKey = SubjectRowId . srId

instance Beamable CourseRowT
instance Beamable (PrimaryKey CourseRowT)

instance Beamable SubjectRowT
instance Beamable (PrimaryKey SubjectRowT)

educatorSchema :: DatabaseSettings be EducatorSchema
educatorSchema = defaultDbSettings

-----------------
-- Example end --
-----------------

-- * Indices definition

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

-- Written this way to make GHC resolve @f@ automatically at a call site.
-- | Instance for @table (TableField table) -> TableField table a@.
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

-- | Make a table index.
tableIndex :: IndexBuilder table a => a -> TableSettings table -> SqlTableIndex
tableIndex = flip buildIndex

mem :: [SqlIndex]
mem = withTableIndex (esSubjects educatorSchema) $
    [ tableIndex (srCourse)
    ]

-- | Generic helper for 'AutoTableindices'.
class GAutoTableIndices be db x where
    autoTableIndices' :: Proxy x -> DatabaseSettings be db -> DList SqlIndex
instance GAutoTableIndices be db (x p) => GAutoTableIndices be db (M1 i f x p) where
    autoTableIndices' _ = autoTableIndices' (Proxy @(x p))
instance (GAutoTableIndices be db (x p), GAutoTableIndices be db (y p)) =>
          GAutoTableIndices be db ((x :*: y) p) where
    autoTableIndices' _ = autoTableIndices' (Proxy @(x p)) <> autoTableIndices' (Proxy @(y p))
instance GAutoTableIndices be db (Rec0 x p) where
    autoTableIndices' _ = mempty
instance {-# OVERLAPPING #-}
         GetDbEntity TableEntity tbl be db =>
         GAutoTableIndices be db (Rec0 (PrimaryKey tbl f) p) where
    autoTableIndices' _ dbSettings = do
        DatabaseEntity (DatabaseTable tblNm tableSettings) <-
            pure $ getDbEntity (Proxy @TableEntity) (Proxy @tbl) dbSettings
        let referedFields = primaryKey tableSettings
            fieldNames = allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) referedFields
        -- we allow ourselves not to arise a compile-time error when the primary key is empty,
        -- the user will have even larger problems in such case anyway.
        maybe DL.empty (DL.singleton . SqlIndex tblNm . SqlTableIndex) (nonEmpty fieldNames)

-- | Traverses the given part of table and for every field which is some 'PrimaryKey'
-- makes corresponding SQL index in the referred table.
-- If a foreign key cannot be resolved within the given database, compile error arises.
class AutoTableIndices be db entity where
    autoTableIndices :: Proxy entity -> DatabaseSettings be db -> DList SqlIndex
-- TODO: not only TableEntity
instance (GAutoTableIndices be db (Rep (TableSettings table) ())) =>
         AutoTableIndices be db (DatabaseEntity be db (TableEntity table)) where
    autoTableIndices _ = autoTableIndices' (Proxy @(Rep (TableSettings table) ()))

qeq =
    let p = Proxy
        _ = esCourses educatorSchema `asProxyTypeOf` p
    in autoTableIndices @Int @EducatorSchema @(DatabaseEntity Int EducatorSchema (TableEntity CourseRowT))  Proxy educatorSchema

-- | Traverses all tables in database and builds indices for all encountered 'PrimaryKey's.
class GAutoDbIndices be db x where
    autoDbIndices' :: Proxy x -> DatabaseSettings be db -> DList SqlIndex
instance GAutoDbIndices be db (x p) => GAutoDbIndices be db (M1 i f x p) where
    autoDbIndices' _ = autoDbIndices' (Proxy @(x p))
instance (GAutoDbIndices be db (x p), GAutoDbIndices be db (y p)) =>
         GAutoDbIndices be db ((x :*: y) p) where
    autoDbIndices' _ = autoDbIndices' (Proxy @(x p)) <> autoDbIndices' (Proxy @(y p))
instance AutoTableIndices be db x => GAutoDbIndices be db (Rec0 x p) where
    autoDbIndices' _ dbSettings = autoTableIndices (Proxy @x) dbSettings

-- | Automatically creates indices for every 'PrimaryKey' embedded into a table.
-- Resulting indices appear exactly in the order in which 'PrimaryKey's are encountered in
-- the database. Indices may repeat (TODO: note that it is okay).
defaultDbIndices
    :: forall be db.
       GAutoDbIndices be db (Rep (DatabaseSettings be db) ())
    => DatabaseSettings be db -> [SqlIndex]
defaultDbIndices db =
    toList $ autoDbIndices' @_ @_ @(Rep (DatabaseSettings be db) ()) Proxy db

meme = defaultDbIndices educatorSchema
