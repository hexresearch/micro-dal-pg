{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Data.DAL.KeyValue.Postgres
( PGEngine'(..)
, PGEngine(..)
, PGEngineSingleConnection(..)
, PGEngineOpts(..)
, createEngine
, withPGEngineSingleConnection
, withPGEngineTransaction
) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Either
import Data.Int
import Data.Pool
import Data.Proxy
import Data.Store
import Data.String (IsString(..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock
import Data.Word
import Database.PostgreSQL.Simple as PGSimple
import Safe
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Set as S

import Data.DAL


newtype PGKey a = PGKey Text
  deriving (Eq, Ord, Show)

unPGKey :: PGKey a -> Text
unPGKey (PGKey k) = k

type PGEngineSingleConnection = PGEngine' Connection
type PGEngine = PGEngine' (Pool Connection)
data PGEngine' c = PGEngine
  { pgEngine'conn     :: c
  , pgEngine'nsexists :: MVar (S.Set Text)
  }

nsUnpackNorm :: NS a -> String
nsUnpackNorm = nsNorm . nsUnpack

nsNorm :: String -> String
nsNorm = replaceSyms "/-" '_'
  where
    replaceSyms what replacement = fmap $ \c -> if c `elem` what then replacement else c

data PGEngineOpts = PGEngineOpts
                        { pgHost      :: Text
                        , pgPort      :: Word16
                        , pgDbName    :: Text
                        , pgUser      :: Text
                        , pgPassword  :: Text
                        , pgPoolSize  :: Int
                        }
    deriving (Eq, Ord, Show)


class HasConnection a where
    withConnection :: a -> (Connection -> IO b) -> IO b

instance HasConnection PGEngineSingleConnection where
    withConnection PGEngine {..} act = act pgEngine'conn

instance HasConnection PGEngine where
    withConnection PGEngine {..} act = withResource pgEngine'conn act

createEngine :: PGEngineOpts -> IO PGEngine
createEngine PGEngineOpts {..} = do
    -- https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html#t:Pool
    pgEngine'conn     <- createPool createPgConn PGSimple.close 1 60 pgPoolSize
    pgEngine'nsexists <- newMVar mempty
    pure PGEngine {..}
    where
      createPgConn = PGSimple.connect $ ConnectInfo
        { connectHost     = cs pgHost
        , connectPort     = pgPort
        , connectUser     = cs pgUser
        , connectPassword = cs pgPassword
        , connectDatabase = cs pgDbName
        }

instance (Store a, HasKey a, HasConnection (PGEngine' c))
  => SourceListAll a IO (PGEngine' c) where

  listAll :: PGEngine' c -> IO [a]
  listAll eng = do
      withConnection eng $ \conn -> do
          rows <- withCreateTable eng conn table $
              query_ conn [qc|select v from {table}|] :: IO [Only (Binary ByteString)]
          pure $ rights $ fmap (\(Only x) -> decode @a (fromBinary x)) rows
    where
      table = nsUnpackNorm (ns @a)

instance (Store a, Store (KeyOf a), HasKey a, HasConnection (PGEngine' c))
  => SourceStore a IO (PGEngine' c) where

  load :: (PGEngine' c) -> KeyOf a -> IO (Maybe a)
  load eng k = do
      withConnection eng $ \conn -> do
          bs <- withCreateTable eng conn table $
              query conn [qc|select v from {table} where k = ?|] (Only (Binary $ encode k)) :: IO [Only (Binary ByteString)]
          case headMay bs of
            Just (Only v) -> pure $ either (const Nothing) (Just) (decode @a (fromBinary v))
            _             -> pure Nothing
    where
      table = nsUnpackNorm (ns @a)

  store :: (PGEngine' c) -> a -> IO (KeyOf a)
  store eng v = do
      withConnection eng $ \conn -> do
          withCreateTable eng conn table $
              execute conn [qc|insert into {table} (k,v) values(?,?) on conflict (k) do update set v=excluded.v|] (bkey,bval)
      pure (key v)
    where
      table = nsUnpackNorm (ns @a)
      bkey  = Binary $ encode (key v)
      bval  = Binary $ encode v

withCreateTable :: (PGEngine' c) -> Connection -> String -> IO a -> IO a
withCreateTable eng conn table ioa = do
    ensureTableExists table
    catch ioa $ \case
        SqlError {sqlState = "42P01"} -> do
            createTable conn table
            ioa
        err -> throwIO err
    where
      ensureTableExists :: String -> IO ()
      ensureTableExists table = do
          tables <- readMVar (pgEngine'nsexists eng)
          when (not $ (cs table) `S.member` tables) $ do
              createTable conn table
              modifyMVar_ (pgEngine'nsexists eng) $ pure . S.insert (cs table)

createTable :: Connection -> String -> IO ()
createTable conn table = void $ execute_ conn $ fromString
    [qc|create table if not exists {table} (k bytea primary key, v bytea)|]

instance (Store a, Store (KeyOf a), HasKey a, HasConnection (PGEngine' c))
  => SourceDeleteByKey a IO (PGEngine' c) where

  delete :: (PGEngine' c) -> KeyOf a -> IO ()
  delete eng k =
      withConnection eng $ \conn -> do
          void $ execute conn [qc|delete from {table} where k = ?|] (Only (Binary $ encode k))
    where
      table = nsUnpackNorm (ns @a)

instance forall a c. (Store a, Store (KeyOf a), HasKey a, HasConnection (PGEngine' c))
  => SourceDeleteAll a IO (PGEngine' c) where
  deleteAll :: Proxy a -> (PGEngine' c) -> IO ()
  deleteAll _ eng =
      withConnection eng $ \conn -> do
          void $ withCreateTable eng conn table $
              execute_ conn [qc|delete from {table}|]
    where
      table = nsUnpackNorm (ns @a)

instance forall a c eng. (Store a, Store (KeyOf a), HasKey a, HasConnection eng, eng ~ PGEngine' c) => SourceCountAll a IO eng where
  countAll :: Proxy a -> eng -> IO Int64
  countAll _ eng = do
      withConnection eng $ \conn -> do
          catch (fmap (fromOnly . head) $ query_ conn [qc|select count(*) from {table}|])
              $ \case
                  SqlError {sqlState = "42P01"} -> pure 0
                  err -> throwIO err
    where
       table = nsUnpackNorm (ns @a)

instance SourceTransaction a IO PGEngineSingleConnection where
  withTransaction eng eff =
      withConnection eng $ \conn -> do
          PGSimple.withTransaction conn eff

withPGEngineSingleConnection :: PGEngine -> (PGEngineSingleConnection -> IO a) -> IO a
withPGEngineSingleConnection eng@PGEngine{..} act = do
    withConnection eng $ \pgEngine'conn -> act PGEngine {..}

withPGEngineTransaction :: PGEngine -> (PGEngineSingleConnection -> IO a) -> IO a
withPGEngineTransaction engp act =
    withPGEngineSingleConnection engp $ \eng -> Data.DAL.withTransaction eng (act eng)
