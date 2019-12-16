module Heart.App.Logging
  ( SimpleLogAction
  , HasSimpleLog (..)
  , WithSimpleLog
  , logMsg
  , log
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  ) where

import Colog.Message (Msg (..))
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Heart.App.Prelude

type SimpleLogAction = LogAction IO Message

class HasSimpleLog env where
  simpleLogL :: Lens' env SimpleLogAction

instance HasSimpleLog SimpleLogAction where
  simpleLogL = simple

type WithSimpleLog env m = (MonadIO m, MonadReader env m, HasSimpleLog env, HasCallStack)

logMsg :: WithSimpleLog env m => Message -> m ()
logMsg msg = do
  LogAction act <- view simpleLogL
  liftIO (act msg)

log :: WithSimpleLog env m => Severity -> Text -> m ()
log sev txt = withFrozenCallStack (logMsg Msg { msgStack = callStack, msgSeverity = sev, msgText = txt })

logDebug :: WithSimpleLog env m => Text -> m ()
logDebug = withFrozenCallStack (log Debug)

logInfo :: WithSimpleLog env m => Text -> m ()
logInfo = withFrozenCallStack (log Info)

logWarning :: WithSimpleLog env m => Text -> m ()
logWarning = withFrozenCallStack (log Warning)

logError :: WithSimpleLog env m => Text -> m ()
logError = withFrozenCallStack (log Error)

logException :: forall e m env . (WithSimpleLog env m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . Text.pack . displayException)
