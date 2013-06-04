{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Config 
    ( RedisAuth (..)
    , RedisConf (..)
    , R.RedisCtx
    , R.Redis
    , R.Connection
    , R.PortID (..)
    , RedisT
    , module Database.Persist
    ) where

import Database.Persist
import qualified Database.Redis as R
import Data.Text (Text, unpack, pack)
import Data.Aeson (Value (Object, Number, String), (.:?), (.!=), FromJSON(..))
import Control.Monad (mzero, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Applicative (Applicative (..))

import Control.Monad.Reader(ReaderT(..))
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Number

newtype RedisAuth =  RedisAuth Text deriving (Eq, Show)

data RedisConf = RedisConf {
    rdHost    :: Text,
    rdPort    :: R.PortID,
    rdAuth    :: Maybe RedisAuth,
    rdMaxConn :: Int
} deriving (Show)

instance FromJSON R.PortID where
    parseJSON (Number (I x)) = (return . R.PortNumber . fromInteger) x
    parseJSON _ = fail "couldn't parse port number"

instance FromJSON RedisAuth where
    parseJSON (String t) = (return . RedisAuth) t
    parseJSON _ = fail "couldn't parse auth" 

newtype RedisT m a = RedisT (ReaderT R.Connection m a)
    deriving (Monad, MonadIO, MonadTrans, Functor, Applicative, MonadPlus)

runRedisPool :: RedisConf -> RedisT m a -> R.Connection -> m a
runRedisPool _ (RedisT r) = runReaderT r

instance PersistConfig RedisConf where
    type PersistConfigBackend RedisConf = RedisT 
    type PersistConfigPool RedisConf = R.Connection

    loadConfig (Object o) = do
        host               <- o .:? "host" .!= R.connectHost R.defaultConnectInfo
        port               <- o .:? "port" .!= R.connectPort R.defaultConnectInfo
        mPass              <- o .:? "password"
        maxConn            <- o .:? "maxConn" .!= R.connectMaxConnections R.defaultConnectInfo

        return RedisConf {
            rdHost = pack host,
            rdPort = port,
            rdAuth = mPass,
            rdMaxConn = maxConn
        }

    loadConfig _ = mzero

    createPoolConfig (RedisConf h p Nothing m) = 
        R.connect $ 
        R.defaultConnectInfo {
            R.connectHost = unpack h,
            R.connectPort = p,
            R.connectMaxConnections = m
        }
    createPoolConfig (RedisConf h p (Just (RedisAuth pwd)) m) = 
        R.connect $ 
        R.defaultConnectInfo {
            R.connectHost = unpack h,
            R.connectPort = p,
            R.connectAuth = Just $ B.pack $ unpack pwd,
            R.connectMaxConnections = m
        }

    runPool = runRedisPool

