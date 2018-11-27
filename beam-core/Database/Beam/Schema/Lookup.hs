{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Schema.Lookup
    ( GGetDbEntity (..)
    , GetDbEntity (..)
    ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector.Sized as VS

import GHC.Exts (Constraint)
import GHC.Generics hiding (C, R)
import GHC.Generics (Generic)
import qualified GHC.Generics as Generic
import GHC.TypeLits

import Database.Beam.Schema.Tables

-- | Filling sized vector conditionally.
class VectorFromOne a b where
    vectorFromOne :: a -> VS.Vector (VectorFromOneSize a b) b

type family VectorFromOneSize a b where
    VectorFromOneSize a a = 1
    VectorFromOneSize a b = 0

instance (VectorFromOneSize a b ~ 0) => VectorFromOne a b where
    vectorFromOne _ = VS.empty

instance {-# OVERLAPPING #-} VectorFromOne a a where
    vectorFromOne = VS.singleton

-- | Generic helper for 'GGetDbEntity'.
class GGetDbEntity table db where
    -- | Number of found tables.
    type GetDbTableNum table db :: Nat
    -- | Find the table in database by given type of the table.
    getDbEntity' :: Proxy table -> db -> VS.Vector (GetDbTableNum table db) table

instance GGetDbEntity table (db p) => GGetDbEntity table (M1 _t _i db p) where
    type GetDbTableNum table (M1 _t _i db p) = GetDbTableNum table (db p)
    getDbEntity' tblP (M1 db) = getDbEntity' tblP db

instance (GGetDbEntity table (x p), GGetDbEntity table (y p)) =>
         GGetDbEntity table ((x :*: y) p) where
    type GetDbTableNum table ((x :*: y) p) =
        GetDbTableNum table (x p) + GetDbTableNum table (y p)
    getDbEntity' tblP (x :*: y) = getDbEntity' tblP x VS.++ getDbEntity' tblP y

instance VectorFromOne table' table => GGetDbEntity table (Rec0 table' p) where
    type GetDbTableNum table (Rec0 table' p) = VectorFromOneSize table' table
    getDbEntity' _ (K1 db) = vectorFromOne db

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
                   ':<>: 'Text " tables of such type.")

-- | Constrain the number of found tables to exactly one.
type GetDbTableAssertOneFound db table found =
    (GetDbTableAssertOneFound' db table found, found ~ 1)

-- | Lookup for a field item in the given database.
-- If an item cannot be resolved, compile error arises.
class GetDbEntity entity (tbl :: (* -> *) -> *) be db  where
    getDbEntity
        :: Proxy entity
        -> Proxy tbl
        -> DatabaseSettings be db
        -> DatabaseEntity be db (entity tbl)

instance (dbSettings ~ DatabaseSettings be db,
          table ~ DatabaseEntity be db (entity tbl),
          Generic dbSettings,
          GGetDbEntity table (Rep dbSettings ()),
          GetDbTableAssertOneFound db tbl (GetDbTableNum table (Rep dbSettings ()))
         ) =>
         GetDbEntity entity tbl be db where
    getDbEntity _ _ dbSettings =
        VS.head $ getDbEntity' (Proxy @table) (Generic.from @_ @() dbSettings)

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


mim = getDbEntity @TableEntity @SubjectRowT Proxy Proxy educatorSchema
