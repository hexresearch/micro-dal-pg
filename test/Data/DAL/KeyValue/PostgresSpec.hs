module Data.DAL.KeyValue.PostgresSpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Store
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Test.Hspec (Spec, SpecWith, hspec, before, describe, it, shouldBe)
import Test.Hspec.NeedEnv (EnvMode(Want), needEnv, needEnvRead)

import Data.DAL
import Data.DAL.KeyValue.HashRef
import Data.DAL.KeyValue.Postgres

data SomeData = SomeData Word32 Word32
                deriving (Eq,Ord,Show,Data,Generic)

instance Store SomeData

instance HasKey SomeData where

  newtype KeyOf SomeData = SomeDataKey Word32
                           deriving (Eq,Ord,Show,Generic,Store)

  key (SomeData a _) = SomeDataKey a
  ns = "somedata"

newtype SomeOtherData = SomeOtherData String
                        deriving (Eq,Ord,Show,Generic,Store)

type HashedInt = HashRef "ints" Int
instance Store HashedInt

instance Arbitrary SomeData where
  arbitrary = SomeData <$> arbitrary <*> arbitrary

getEnvs :: IO PGEngineOpts
getEnvs = do
    pgHost      <- cs <$> needEnv mode "TEST_PG_DBHOST"
    pgPort      <-    needEnvRead mode "TEST_PG_DBPORT"
    pgUser      <- cs <$> needEnv mode "TEST_PG_DBUSER"
    pgPassword  <- cs <$> needEnv mode "TEST_PG_DBPASSWORD"
    pgDbName    <- cs <$> needEnv mode "TEST_PG_DBNAME"
    pgPoolSize  <- pure 3
    pure PGEngineOpts {..}
    where
      mode = Want

spec :: Spec
spec = before (createEngine =<< getEnvs) specWithPG

specWithPG :: SpecWith PGEngine
specWithPG = do

  describe "DAL PG simple load/store test" $ do
    it "stores some random SomeData values and restores them" $ \eng' -> do

      replicateM_ 100 $ do
        withPGEngineSingleConnection eng' $ \eng -> withTransaction eng $ do

          v1 <- generate arbitrary :: IO SomeData
          k1 <- store eng v1
          v2 <- load eng k1

          v2 `shouldBe` Just v1

  describe "DAL PG simple store/loadAll test" $ do
    it "stores some random SomeData values and restores them" $ \eng' -> do

      replicateM_ 100 $ do
        withPGEngineSingleConnection eng' $ \eng -> withTransaction eng $ do
          deleteAll (Proxy @SomeData) eng

          els  <- Map.fromList <$> generate arbitrary :: IO (Map Word32 Word32)
          let vals  = [ SomeData k v | (k,v) <- Map.toList els ]
          mapM (store eng) vals
          vals2 <- listAll @SomeData eng

          (sort vals2) `shouldMatchList` (sort vals)


  describe "DAL PG HashRef test" $ do
    it "stores and restores some random values using HashRef" $ \eng' -> do

      replicateM_ 10 $ do
        withPGEngineSingleConnection eng' $ \eng -> withTransaction eng $ do

          ivalues <- generate arbitrary :: IO [Int]
          forM_ ivalues $ \i -> do
            k <- store @HashedInt eng (hashRefPack i)
            ii <- load @HashedInt eng k
            Just i `shouldBe` (fromJust $ hashRefUnpack <$> ii)


  describe "DAL PG delete test" $ do
    it "stores and restores some random values using HashRef and deletes odds" $ \eng' -> do

      replicateM_ 10 $ do
        withPGEngineSingleConnection eng' $ \eng -> withTransaction eng $ do
          deleteAll (Proxy @HashedInt) eng

          let ivalues = [1..200]

          forM_ ivalues $ \i -> do
            store @HashedInt eng (hashRefPack i)

          hvals <- listAll @HashedInt eng
          ivals <- (catMaybes . fmap hashRefUnpack) <$> pure hvals

          sort ivalues `shouldMatchList` sort ivals

          forM_ (filter odd ivalues) $ \v -> do
            delete @HashedInt eng (key (hashRefPack v))

          ivals2 <- fmap sort $ (catMaybes . fmap hashRefUnpack) <$> listAll @HashedInt eng
          ivals2 `shouldMatchList` (filter even ivalues)
