module VM.Platform.Debug (
    DebugPlatform,
    newDebugPlatform
) where

import Control.Applicative
import Data.IORef

import VM.Data


data DebugPlatform = DebugPlatform (IORef String)

instance Platform DebugPlatform where
    setTime (DebugPlatform ref) time = modifyIORef ref (++ (show time ++ "\n"))
    output (DebugPlatform ref)       = readIORef ref
    lookupPlatformNamespace _ _      = return Nothing
    lookupPlatformMetaNamespace _ _  = return Nothing

newDebugPlatform :: IO DebugPlatform
newDebugPlatform = DebugPlatform <$> newIORef "time change log\n"

