{-# LANGUAGE UndecidableInstances #-}

module Heart.App.App
  ( App (..)
  , AppWrapper (..)
  , appLogAction
  , appStore
  , HasApp (..)
  , newApp
  ) where

import Colog.Actions (richMessageAction)
import Colog.Message (Message)
import Heart.App.Logging (HasSimpleLog (..))
import Heart.App.Prelude
import Heart.App.Stats (HasStore (..), Store, newStore)

data App = App
  { _appLogAction :: !(LogAction IO Message)
  , _appStore     :: !Store
  }

$(makeLenses ''App)

class HasApp env where
  appL :: Lens' env App

instance HasApp App where
  appL = simple

instance HasSimpleLog App where
  simpleLogL = appLogAction

instance HasStore App where
  storeL = appStore

newApp :: MonadIO m => m App
newApp = App richMessageAction <$> newStore

newtype AppWrapper env = AppWrapper env

instance HasApp (AppWrapper env) => HasSimpleLog (AppWrapper env) where
  simpleLogL = appL . simpleLogL

instance HasApp (AppWrapper env) => HasStore (AppWrapper env) where
  storeL = appL . storeL
