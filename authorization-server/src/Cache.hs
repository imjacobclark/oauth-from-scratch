module Cache where

import Data.Cache
import Models.Client

newClientCache :: IO (Cache String Client)
newClientCache = do
    c <- newCache Nothing
    return c

insertInflightClientIntoCache :: (Cache String Client) -> Client -> String -> IO ()
insertInflightClientIntoCache cacheClient client requestId = insert cacheClient requestId client

findInflightClientInCache :: (Cache String Client) -> String -> IO (Maybe Client)
findInflightClientInCache cacheClient requestId = do
    inflightAuthorization <- Data.Cache.lookup cacheClient requestId
    return inflightAuthorization