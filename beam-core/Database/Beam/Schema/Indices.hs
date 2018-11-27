{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Database.Beam.Schema.Indices
    ( SqlTableIndex
    , IndexBuilder (..)
    , tableIndex
    , withTableIndex
    , createIndex

    ) where

import Data.DList (DList)
import qualified Data.DList as DL
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Exts (toList)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)

#if !MIN_VERSION_base(4, 11, 0)
import Control.Monad.Writer (execWriter, tell)
import Data.Semigroup
#endif

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

class IndexBuilder table a where
    buildIndex :: TableSettings table -> a -> SqlTableIndex

-- Written this way to make GHC resolve @f@ automatically at a call site.
-- | Instance for @table (TableField table) -> TableField table a@.
instance (f ~ TableField table, field ~ Columnar f a) =>
         IndexBuilder table (table f -> field) where
    buildIndex settings getter =
        SqlTableIndex . (:| []) . _fieldName $ getter settings

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
mem = withTableIndex (esCourses educatorSchema) $
    [ tableIndex (crId, crDesc)
    ]

-- | Traverses the given part of table and for every field which is some 'PrimaryKey'
-- makes corresponding SQL index in the referred table.
-- If a foreign key cannot be resolved within the given database, compile error arises.
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
            fieldNames = execWriter $ zipBeamFieldsM mergeReferedFields referedFields referedFields
        -- we allow ourselves not to arise a compile-time error when the primary key is empty,
        -- the user will have even larger problems in such case anyway.
        maybe DL.empty (DL.singleton . SqlIndex tblNm . SqlTableIndex) (nonEmpty fieldNames)
      where
        mergeReferedFields (Columnar' (TableField fieldNm)) fields =
            tell [fieldNm] $> fields

class AutoTableIndices be db entity where
    autoTableIndices :: Proxy entity -> DatabaseSettings be db -> DList SqlIndex
instance (GAutoTableIndices be db (Rep (TableSettings table) ())) =>
         AutoTableIndices be db (DatabaseEntity be db (TableEntity table)) where
    autoTableIndices _ = autoTableIndices' (Proxy @(Rep (TableSettings table) ()))

qeq =
    let p = Proxy
        _ = esCourses educatorSchema `asProxyTypeOf` p
    in autoTableIndices @Int @EducatorSchema @(DatabaseEntity Int EducatorSchema (TableEntity CourseRowT))  Proxy educatorSchema

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
defaultDbIndices
    :: forall be db.
       GAutoDbIndices be db (Rep (DatabaseSettings be db) ())
    => DatabaseSettings be db -> [SqlIndex]
defaultDbIndices db =
    ordNub . toList $ autoDbIndices' @_ @_ @(Rep (DatabaseSettings be db) ()) Proxy db
  where
    ordNub = S.toList . S.fromList

meme = defaultDbIndices educatorSchema
