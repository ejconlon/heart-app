module Heart.App.Stats
  ( Counter
  , HasStore (..)
  , Server
  , Store
  , forkServer
  , killServer
  , newCounter
  , newStore
  , registerCounter
  , registerGcMetrics
  , incCounter
  , readCounter
  ) where

import Control.Concurrent (killThread)
import Data.Text.Encoding (encodeUtf8)
import Heart.App.Prelude
import System.Metrics (Store)
import qualified System.Metrics as M
import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as C
import System.Remote.Monitoring (Server, forkServerWith, serverThreadId)

class HasStore env where
  storeL :: Lens' env Store

instance HasStore Store where
  storeL = simple

newStore :: MonadIO m => m Store
newStore = liftIO M.newStore

registerGcMetrics :: (MonadReader env m, HasStore env, MonadIO m) => m ()
registerGcMetrics = do
  store <- view storeL
  liftIO (M.registerGcMetrics store)

forkServer :: (MonadReader env m, HasStore env, MonadIO m) => Text -> Int -> m Server
forkServer host port = do
  store <- view storeL
  liftIO (forkServerWith store (encodeUtf8 host) port)

killServer :: MonadIO m => Server -> m ()
killServer server = liftIO (killThread (serverThreadId server))

newCounter :: MonadIO m => m Counter
newCounter = liftIO C.new

registerCounter :: (MonadReader env m, HasStore env, MonadIO m) => Text -> Getter env Counter -> m ()
registerCounter name lenz = do
  store <- view storeL
  v <- view lenz
  liftIO (M.registerCounter name (C.read v) store)

incCounter :: (MonadReader env m, MonadIO m) => Getter env Counter -> m ()
incCounter lenz = view lenz >>= liftIO . C.inc

readCounter :: (MonadReader env m, MonadIO m) => Getter env Counter -> m Int64
readCounter lenz = view lenz >>= liftIO . C.read
