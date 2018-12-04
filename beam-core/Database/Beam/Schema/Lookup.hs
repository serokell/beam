{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Schema.Lookup
    ( GGetDbEntity (..)
    , GetDbEntity (..)
    , module Database.Beam.Schema.Lookup
    ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector.Sized as VS
import Lens.Micro hiding (to)

import GHC.Exts (Constraint)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)
import qualified GHC.Generics as Generic
import GHC.TypeLits

import Database.Beam.Schema.Tables

-- | Filling sized vector conditionally.
class VectorFromOne a b where
    vectorFromOne :: a -> VS.Vector (VectorFromOneSize a b) b
    vectorFromOneSym :: b -> VS.Vector (VectorFromOneSize a b) a

type family VectorFromOneSize a b where
    VectorFromOneSize a a = 1
    VectorFromOneSize a b = 0

instance (VectorFromOneSize a b ~ 0) => VectorFromOne a b where
    vectorFromOne _ = VS.empty
    vectorFromOneSym _ = VS.empty

instance {-# OVERLAPPING #-} VectorFromOne a a where
    vectorFromOne = VS.singleton
    vectorFromOneSym = VS.singleton

-- | Generic helper for 'GGetDbEntity'.
class GGetDbEntity table db where
    -- | Number of found tables.
    type GetDbTableNum table db :: Nat
    -- | Find the table in the database by given type of the table.
    --   Return all found options.
    getDbEntity' :: Proxy table -> db -> VS.Vector (GetDbTableNum table db) table
    -- | Replace a table in the database with matching type with the given one.
    --   Return all found options.
    overDbEntity' :: Proxy table -> db -> table -> VS.Vector (GetDbTableNum table db) db

instance GGetDbEntity table (db p) => GGetDbEntity table (M1 _t _i db p) where
    type GetDbTableNum table (M1 _t _i db p) = GetDbTableNum table (db p)
    getDbEntity' tblP (M1 db) = getDbEntity' tblP db
    overDbEntity' tblP (M1 db) table = fmap M1 $ overDbEntity' tblP db table

instance (GGetDbEntity table (x p), GGetDbEntity table (y p)) =>
         GGetDbEntity table ((x :*: y) p) where
    type GetDbTableNum table ((x :*: y) p) =
        GetDbTableNum table (x p) + GetDbTableNum table (y p)
    getDbEntity' tblP (x :*: y) = getDbEntity' tblP x VS.++ getDbEntity' tblP y
    overDbEntity' tblP (x :*: y) table =
        fmap (:*: y) (overDbEntity' tblP x table)
        VS.++
        fmap (x :*:) (overDbEntity' tblP y table)

instance VectorFromOne table' table => GGetDbEntity table (Rec0 table' p) where
    type GetDbTableNum table (Rec0 table' p) = VectorFromOneSize table' table
    getDbEntity' _ (K1 db) = vectorFromOne db
    overDbEntity' _ (K1 _) table = K1 <$> vectorFromOneSym table

type family GetDbTableAssertOneFound' (db :: (* -> *) -> *)
                                      (table :: (* -> *) -> *)
                                      (found :: Nat) :: Constraint where
    GetDbTableAssertOneFound' db table 0 =
        TypeError ('Text "Unknown foreign key reference, table `"
                   ':<>: 'ShowType table
                   ':<>: 'Text "' is not found in database `"
                   ':<>: 'ShowType db
                   ':<>: 'Text "'.")
    GetDbTableAssertOneFound' _ _ 1 = ()
    GetDbTableAssertOneFound' db table n =
        TypeError ('Text "Ambiguous reference to table `"
                   ':<>: 'ShowType table
                   ':<>: 'Text "'"
                   ':$$: 'Text "Database `"
                   ':<>: 'ShowType db
                   ':<>: 'Text "' has "
                   ':<>: 'ShowType n
                   ':<>: 'Text " tables of such type."
                   ':$$: 'Text "Hint: consider adding a phantom type parameter to the "
                   ':<>: 'Text "table datatype so that all its occurrences get "
                   ':<>: 'Text "distinguishable.")

-- | Constrain the number of found tables to exactly one.
type GetDbTableAssertOneFound db table found =
    (GetDbTableAssertOneFound' db table found, found ~ 1)

-- | Lookup for a field item in the given database.
-- If an item cannot be resolved, compile error arises.
class GetDbEntity entity dbEntity (tbl :: (* -> *) -> *) be db where
    getDbEntity
        :: Proxy entity
        -> Proxy tbl
        -> db (dbEntity be db)
        -> dbEntity be db (entity tbl)
    overDbEntity
        :: Proxy entity
        -> Proxy tbl
        -> db (dbEntity be db)
        -> dbEntity be db (entity tbl)
        -> db (dbEntity be db)
    dbEntityL
        :: Proxy entity
        -> Proxy tbl
        -> Lens' (db (dbEntity be db)) (dbEntity be db (entity tbl))

instance (dbSettings ~ db (dbEntity be db),
          table ~ dbEntity be db (entity tbl),
          Generic dbSettings,
          GGetDbEntity table (Rep dbSettings ()),
          GetDbTableAssertOneFound db tbl (GetDbTableNum table (Rep dbSettings ()))
         ) =>
         GetDbEntity entity dbEntity tbl be db where
    getDbEntity _ _ dbSettings =
        VS.head $ getDbEntity' (Proxy @table) (Generic.from @_ @() dbSettings)
    overDbEntity _ _ dbSettings table =
        Generic.to . VS.head $
            overDbEntity' (Proxy @table) (Generic.from @_ @() dbSettings) table
    dbEntityL entity tbl = lens (getDbEntity entity tbl) (overDbEntity entity tbl)

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


mim = getDbEntity @TableEntity @DatabaseEntity @SubjectRowT Proxy Proxy educatorSchema
