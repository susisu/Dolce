module VM.Platform.DOS (
    DOSPlatform,
    newDOSPlatform
) where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import qualified Data.HashTable.IO as H
import Data.IORef
import qualified Data.Map as M

import VM
import VM.Data


type HashTable k v = H.BasicHashTable k v


type Col = Int

type NoteType = Int

type Frame = Int


data DOSPlatform = DOSPlatform {
        currentTime   :: IORef Double,
        noteEvents    :: IOArray (Col, NoteType) [NoteEvent],
        eventNames    :: IORef [String],
        events        :: HashTable String [Event],
        userNameSpace :: HashTable String Value,
        namespace     :: M.Map String Value,
        metaNamespace :: M.Map String Value
    }

instance Platform DOSPlatform where
    setTime p time                      = writeIORef (currentTime p) time
    output p                            = do
        result <- newIORef ""
        names  <- readIORef (eventNames p)
        forM_ names $ \name -> do
            queue <- H.lookup (events p) name
            case queue of
                Just queue -> modifyIORef result (++ name ++ show (reverse queue) ++ "\n")
                Nothing    -> return ()
        readIORef result
    lookupPlatformNamespace p name      = do
        value <- H.lookup (userNameSpace p) name
        case value of
            Just _  -> return value
            Nothing -> return $ M.lookup name (namespace p)
    lookupPlatformMetaNamespace p name  = return $ M.lookup name (metaNamespace p)


data NoteEvent = NoteEvent Frame

instance Show NoteEvent where
    show (NoteEvent frame) = show frame

data Event = Event Frame [Value]

instance Show Event where
    show (Event frame values) = show frame ++ "(" ++ show values ++ ")"


newDOSPlatform :: IO DOSPlatform
newDOSPlatform = do
    currentTime   <- newIORef 0.0
    noteEvents    <- newArray ((1, 1), (1, 1)) []
    eventNames    <- newIORef []
    events        <- H.new
    userNameSpace <- H.new
    let
        namespace = M.fromList [
            ]
            where
                _line = FuncValue __line
                __line = undefined
                _put = FuncValue __put
                __put = undefined
        metaNamespace = M.fromList [
                ("event", _event)
            ]
            where
                _event = FuncValue __event
                __event args state = case validateTypes [StrType] [head args] of
                    Left err              -> returnL err
                    Right [StrValue name] -> case validateTypes (replicate (length $ tail args) TypeType) (tail args) of
                        Left (ArgumentError index mes) -> returnL $ ArgumentError (index + 1) mes
                        Right typeValues               -> do
                            let types = map getTypeFromTypeValue $ typeValues
                            modifyIORef eventNames (name :)
                            H.insert events name []
                            H.insert userNameSpace name $ FuncValue $ \args state -> checkTypes types args $ \values -> do
                                    queue <- H.lookup events name
                                    case queue of
                                        Just queue -> do
                                            fps  <- getFPS state
                                            time <- readIORef currentTime
                                            H.insert events name $ Event (round $ time * fps) values : queue
                                            finish
                                        Nothing    -> returnL $ ArgumentError (negate 1) ("unknown event: " ++ name)
                            finish
                    where
                        getTypeFromTypeValue :: Value -> Type
                        getTypeFromTypeValue (TypeValue t) = t
                        getTypeFromTypeValue value         = typeOf value
                _init = FuncValue __init
                __init = undefined
    return $ DOSPlatform {
            currentTime   = currentTime,
            noteEvents    = noteEvents,
            eventNames    = eventNames,
            events        = events,
            userNameSpace = userNameSpace,
            namespace     = namespace,
            metaNamespace = metaNamespace
        }

