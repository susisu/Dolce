{-# LANGUAGE RankNTypes #-}

module VM.Data (
    Type(..),
    Value(..),
    typeOf,
    OperationError(..),
    showAsWarning,
    ArgumentError(..),
    VMState,
    newVMState,
    getFPS,
    setFPS,
    getCorrection,
    setCorrection,
    getInterval,
    setInterval,
    getUnitLength,
    setUnitLength,
    getCurrentTime,
    setCurrentTime,
    modifyCurrentTime,
    getIsInHeader,
    setIsInHeader,
    lookupNamespace,
    defineValue,
    deleteValue,
    lookupMetaNamespace,
    Platform(..)
) where

import Control.Applicative
import qualified Data.HashTable.IO as H
import Data.IORef
import Text.Parsec.Pos (SourcePos)


type HashTable k v = H.BasicHashTable k v


data Type = StrType | NumType | FuncType | TypeType | AnyType deriving (Eq)

instance Show Type where
    show StrType  = "%String%"
    show NumType  = "%Number%"
    show FuncType = "%Function%"
    show TypeType = "%Type%"
    show AnyType  = "%Any%"


data Value =
      StrValue String
    | NumValue Double
    | FuncValue (forall p. Platform p => [Value] -> VMState p -> IO (Either ArgumentError ()))
    | TypeValue Type

instance Show Value where
    show (StrValue str) = "\"" ++ str ++ "\""
    show (NumValue num) = show num
    show (FuncValue _)  = "[Function]"
    show (TypeValue t)  = show t

typeOf :: Value -> Type
typeOf (StrValue _)  = StrType
typeOf (NumValue _)  = NumType
typeOf (FuncValue _) = FuncType
typeOf (TypeValue _) = TypeType


data OperationError = OperationError SourcePos String

instance Show OperationError where
    show (OperationError pos mes) = "error@" ++ show pos ++ ": " ++ mes

showAsWarning :: OperationError -> String
showAsWarning (OperationError pos mes) = "warning@" ++ show pos ++ ": " ++ mes

data ArgumentError = ArgumentError Int String


type Time = Double


data VMState p = VMState {
    -- frames per second
    fps           :: IORef Double,
    -- the correction of time
    correction    :: IORef Time,
    -- the interval between beats
    interval      :: IORef Time,
    -- the length of unit relative to a beat
    unitLength    :: IORef Time,
    -- current time
    currentTime   :: IORef Time,
    -- indicates the state is in header or not
    isInHeader    :: IORef Bool,
    -- the name space for user defined values
    userNamespace :: HashTable String Value,
    -- the platform
    platform      :: p
}

newVMState :: Platform p => p -> IO (VMState p)
newVMState platform = do
        fps           <- newIORef 60.0
        correction    <- newIORef 0.0
        interval      <- newIORef 1.0
        unitLength    <- newIORef 1.0
        currentTime   <- newIORef 0.0
        isInHeader    <- newIORef True
        userNamespace <- H.new
        setTime platform =<< ((+) <$> (readIORef currentTime) <*> (readIORef correction))
        return $ VMState {
            fps           = fps,
            correction    = correction,
            interval      = interval,
            unitLength    = unitLength,
            currentTime   = currentTime,
            isInHeader    = isInHeader,
            userNamespace = userNamespace,
            platform      = platform
        }

getFPS :: VMState p -> IO Double
getFPS state = readIORef (fps state)

setFPS :: VMState p -> Double -> IO ()
setFPS state = writeIORef (fps state)

getCorrection :: VMState p -> IO Time
getCorrection state = readIORef (correction state)

setCorrection :: Platform p => VMState p -> Time -> IO ()
setCorrection state corr = writeIORef (correction state) corr >> updatePlatformTime state

getInterval :: VMState p -> IO Time
getInterval state = readIORef (interval state)

setInterval :: VMState p -> Time -> IO ()
setInterval state = writeIORef (interval state)

getUnitLength :: VMState p -> IO Time
getUnitLength state = readIORef (unitLength state)

setUnitLength :: VMState p -> Time -> IO ()
setUnitLength state = writeIORef (unitLength state)

getCurrentTime :: VMState p -> IO Time
getCurrentTime state = readIORef (currentTime state)

setCurrentTime :: Platform p => VMState p -> Time -> IO ()
setCurrentTime state time = writeIORef (currentTime state) time >> updatePlatformTime state

modifyCurrentTime :: Platform p => VMState p -> (Time -> Time) -> IO ()
modifyCurrentTime state f = modifyIORef (currentTime state) f >> updatePlatformTime state

getIsInHeader :: VMState p -> IO Bool
getIsInHeader state = readIORef (isInHeader state)

setIsInHeader :: VMState p -> Bool -> IO ()
setIsInHeader state = writeIORef (isInHeader state)

lookupNamespace :: Platform p => VMState p -> String -> IO (Maybe Value)
lookupNamespace state name = do
        value <- H.lookup (userNamespace state) name
        case value of
            Just _  -> return value
            Nothing -> lookupPlatformNamespace (platform state) name

defineValue :: VMState p -> String -> Value -> IO ()
defineValue state name value = H.insert (userNamespace state) name value

deleteValue :: VMState p -> String -> IO ()
deleteValue state name = H.delete (userNamespace state) name

lookupMetaNamespace :: Platform p => VMState p -> String -> IO (Maybe Value)
lookupMetaNamespace state = lookupPlatformMetaNamespace (platform state)

updatePlatformTime :: Platform p => VMState p -> IO ()
updatePlatformTime state = setTime (platform state) =<< ((+) <$> getCurrentTime state <*> getCorrection state)


class Platform p where
    setTime                     :: p -> Time -> IO ()
    output                      :: p -> IO String
    lookupPlatformNamespace     :: p -> String -> IO (Maybe Value)
    lookupPlatformMetaNamespace :: p -> String -> IO (Maybe Value)

