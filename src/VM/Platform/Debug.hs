module VM.Platform.Debug (
    DebugPlatform,
    newDebugPlatform
) where

import Data.IORef
import qualified Data.Map as M

import VM
import VM.Data


data DebugPlatform = DebugPlatform {
        logRef    :: IORef String,
        namespace :: M.Map String Value
    }

instance Platform DebugPlatform where
    setTime p time                   = modifyIORef (logRef p) (++ (show time ++ "\n"))
    output p                         = readIORef (logRef p)
    lookupPlatformNamespace p name   = return $ M.lookup name (namespace p)
    lookupPlatformMetaNamespace _ _  = return Nothing

newDebugPlatform :: IO DebugPlatform
newDebugPlatform = do
    logRef <- newIORef "-- time change log --\n"
    let
        namespace = M.fromList [
                ("log", _log)
            ]
        _log = FuncValue __log
        __log args state = checkTypes [AnyType] args $ \[value] ->
            modifyIORef logRef (++ (show value ++ "\n")) >> finish
    return $ DebugPlatform logRef namespace

