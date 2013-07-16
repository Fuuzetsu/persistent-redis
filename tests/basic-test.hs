{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
module Main where

import Database.Redis
import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH
import Database.Persist.Quasi
import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)

let redisSettings = (mkPersistSettings (ConT ''RedisBackend))
 in share [mkPersist redisSettings] [persistLowerCase| 
Person
    name String
    age Int
    deriving Show
|]

d :: ConnectInfo
d = defaultConnectInfo

host :: Text
host = pack $ connectHost d

redisConf :: RedisConf
redisConf = RedisConf host (connectPort d) Nothing 10

main :: IO ()
main = do
    withRedisConn redisConf $ runRedisPool $ do
        s <- insert $ Person "Test" 12
        liftIO $ print s
        insertKey (Key (PersistText "person_test")) $ Person "Test2" 45
        return ()