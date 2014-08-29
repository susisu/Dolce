{-
    Dolce - VM/Platform/DOS.hs
    Copyright (c) 2014, Susisu
    see: license.txt
-}

module VM.Platform.DOS (
    DOSPlatform,
    newDOSPlatform
) where

import Control.Monad
import Data.Array.IO
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified System.IO.UTF8 as UTF8

import VM
import VM.Data


type HashTable k v = H.BasicHashTable k v


type Col = Int

type NoteType = Int

type Frame = Int


type IOArrayRef i e = IORef (IOArray i e)

newIOArrayRef :: Ix i => (i, i) -> e -> IO (IOArrayRef i e)
newIOArrayRef b e = newIORef =<< newArray b e

renewIOArrayRef :: Ix i => IOArrayRef i e -> (i, i) -> e -> IO ()
renewIOArrayRef r b e = writeIORef r =<< newArray b e

readIOArrayRef :: Ix i => IOArrayRef i e -> i -> IO e
readIOArrayRef r i = do
        a <- readIORef r
        readArray a i

writeIOArrayRef :: Ix i => IOArrayRef i e -> i -> e -> IO ()
writeIOArrayRef r i e = do
        a <- readIORef r
        writeArray a i e


data DOSPlatform = DOSPlatform {
        currentTime   :: IORef Double,
        numCols       :: IORef Col,
        numNoteTypes  :: IORef NoteType,
        noteEvents    :: IOArrayRef (Col, NoteType) [NoteEvent],
        columnLayout  :: IORef [Col],
        noteNames     :: IOArrayRef (Col, NoteType) String,
        eventNames    :: IORef [String],
        events        :: HashTable String [Event],
        headerData    :: IORef [Header],
        mergeFiles    :: IORef [FilePath],
        userNameSpace :: HashTable String Value,
        namespace     :: M.Map String Value,
        metaNamespace :: M.Map String Value
    }

instance Platform DOSPlatform where
    setTime p time                      = writeIORef (currentTime p) time
    output p                            = do
            result    <- newIORef ""
            cols      <- readIORef (numCols p)
            noteTypes <- readIORef (numNoteTypes p)
            forM_ [(c, n) | n <- [1 .. noteTypes], c <- [1 .. cols]] $ \index@(c, n) -> do
                    queue <- readIOArrayRef (noteEvents p) index
                    name  <- readIOArrayRef (noteNames p) index
                    let queueStr = concat . intersperse "," . map show $ sort queue
                    if name == "" then
                        modifyIORef result (++ "&data" ++ show c ++ "_" ++ show n ++ "=" ++ queueStr)
                    else
                        modifyIORef result (++ "&" ++ name ++ "=" ++ queueStr)
            names <- readIORef (eventNames p)
            forM_ names $ \name -> do
                    queue <- H.lookup (events p) name
                    case queue of
                        Just queue -> let queueStr = concat . intersperse "," . map show $ sort queue
                            in modifyIORef result (++ "&" ++ name ++ "=" ++ queueStr)
                        Nothing    -> return ()
            headers <- readIORef (headerData p)
            forM_ (reverse headers) $ \(Header name value) ->
                modifyIORef result (++ "&" ++ name ++ "=" ++ value)
            paths <- readIORef (mergeFiles p)
            forM_ (reverse paths) $ \path -> do
                file <- UTF8.readFile path
                modifyIORef result (++ file)
            readIORef result
    lookupPlatformNamespace p name      = do
            value <- H.lookup (userNameSpace p) name
            case value of
                Just _  -> return value
                Nothing -> return $ M.lookup name (namespace p)
    lookupPlatformMetaNamespace p name  = return $ M.lookup name (metaNamespace p)


newtype NoteEvent = NoteEvent Frame deriving (Eq, Ord)

instance Show NoteEvent where
    show (NoteEvent frame) = show frame

data Event = Event Frame [Value]

instance Show Event where
    show (Event frame values) = show frame ++ "," ++ (concat . intersperse "," $ map showValue values)
        where
            showValue :: Value -> String
            showValue (StrValue str) = str
            showValue v              = show v

instance Eq Event where
    (Event a _) == (Event b _) = a == b

instance Ord Event where
    compare (Event a _) (Event b _) = compare a b

data Header = Header String String


newDOSPlatform :: IO DOSPlatform
newDOSPlatform = do
        currentTime   <- newIORef 0.0
        let
            defaultNumCols = 1
            defaultNumNoteTypes = 1
        numCols       <- newIORef defaultNumCols
        numNoteTypes  <- newIORef defaultNumNoteTypes
        noteEvents    <- newIOArrayRef ((1, 1), (defaultNumCols, defaultNumNoteTypes)) []
        columnLayout  <- newIORef [1 .. defaultNumCols]
        noteNames     <- newIOArrayRef ((1, 1), (defaultNumCols, defaultNumNoteTypes)) ""
        eventNames    <- newIORef []
        events        <- H.new
        headerData    <- newIORef []
        mergeFiles    <- newIORef []
        userNameSpace <- H.new
        let
            namespace = M.fromList [
                    ("put"   , _put),
                    ("@"     , _put),
                    ("row"   , _row),
                    ("$"     , _row),
                    ("layout", _layout),
                    ("!"     , _layout)
                ]
                where
                    _layout = FuncValue __layout
                    __layout args _ = do
                            cols      <- readIORef numCols
                            checkTypes (replicate cols NumType) args $ \cs ->
                                do
                                    let
                                        newCols = map (floor . getNumFromValue) cs
                                    res <- foldr joinFuncs finish . (flip map $ zip [0 ..] newCols) $ \(index, col) ->
                                            if col < 1 || cols < col then
                                                returnL $ ArgumentError index "column is out of range"
                                            else
                                                finish
                                    case res of
                                        err@(Left _) -> return err
                                        Right _      -> writeIORef columnLayout newCols >> finish
                    _put = FuncValue __put
                    __put args state = checkTypes [NumType, NumType] args $ \[NumValue c, NumValue n] ->
                        let
                            col      = floor c
                            noteType = floor n
                        in do
                            cols      <- readIORef numCols
                            noteTypes <- readIORef numNoteTypes
                            if col < 1 || cols < col then
                                returnL $ ArgumentError 0 "column is out of range"
                            else if noteType < 1 || noteTypes < noteType then
                                returnL $ ArgumentError 1 "note type is out of range"
                            else do
                                queue <- readIOArrayRef noteEvents (col, noteType)
                                fps   <- getFPS state
                                time  <- readIORef currentTime
                                writeIOArrayRef noteEvents (col, noteType) $ NoteEvent (round $ time * fps) : queue
                                finish
                    _row = FuncValue __row
                    __row args state = do
                            cols      <- readIORef numCols
                            noteTypes <- readIORef numNoteTypes
                            layout    <- readIORef columnLayout
                            checkTypes (replicate cols NumType) args $ \ns ->
                                foldr joinFuncs finish . (flip map $ zip3 [0 ..] (map (floor . getNumFromValue) ns) layout) $ \(index, noteType, col) ->
                                    if col < 1 || cols < col then
                                        returnL $ ArgumentError index "column is out of range"
                                    else if noteType < 1 || noteTypes < noteType then
                                        finish
                                    else do
                                        queue <- readIOArrayRef noteEvents (col, noteType)
                                        fps   <- getFPS state
                                        time  <- readIORef currentTime
                                        writeIOArrayRef noteEvents (col, noteType) $ NoteEvent (round $ time * fps) : queue
                                        finish
                            interval   <- getInterval state
                            unitLength <- getUnitLength state
                            modifyCurrentTime state (+ (interval * unitLength))
                            finish
            getNumFromValue :: Value -> Double
            getNumFromValue (NumValue num) = num
            getNumFromValue _              = 0.0

            metaNamespace = M.fromList [
                    ("init"  , _init),
                    ("event" , _event),
                    ("name"  , _name),
                    ("header", _header),
                    ("merge" , _merge)
                ]
                where
                    _event = FuncValue __event
                    __event args _ = case validateTypes [StrType] [head args] of
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
                        Right _                            -> finish
                        where
                            getTypeFromTypeValue :: Value -> Type
                            getTypeFromTypeValue (TypeValue t) = t
                            getTypeFromTypeValue value         = typeOf value
                    _init = FuncValue __init
                    __init args _ = checkTypes [NumType, NumType] args $ \[NumValue c, NumValue n] ->
                        let
                            cols      = floor c
                            noteTypes = floor n
                        in if cols < 1 then
                            returnL $ ArgumentError 0 "number of columns must be >= 1"
                        else if noteTypes < 1 then
                            returnL $ ArgumentError 1 "number of note types must be >= 1"
                        else do
                                writeIORef numCols cols
                                writeIORef numNoteTypes noteTypes
                                renewIOArrayRef noteEvents ((1, 1), (cols, noteTypes)) []
                                writeIORef columnLayout [1 .. cols]
                                renewIOArrayRef noteNames ((1, 1), (cols, noteTypes)) ""
                                finish
                    _name = FuncValue __name
                    __name args _ = checkTypes [NumType, NumType, StrType] args $ \[NumValue c, NumValue n, StrValue name] ->
                        let
                            col      = floor c
                            noteType = floor n
                        in do
                            cols      <- readIORef numCols
                            noteTypes <- readIORef numNoteTypes
                            if col < 1 || cols < col then
                                returnL $ ArgumentError 0 "column is out of range"
                            else if noteType < 1 || noteTypes < noteType then
                                returnL $ ArgumentError 1 "note type is out of range"
                            else do
                                writeIOArrayRef noteNames (col, noteType) name
                                finish
                    _header = FuncValue __header
                    __header args _ = checkTypes [StrType, StrType] args $ \[StrValue name, StrValue value] ->
                        modifyIORef headerData (Header name value :) >> finish
                    _merge = FuncValue __merge
                    __merge args _ = checkTypes [StrType] args $ \[StrValue path] ->
                        modifyIORef mergeFiles (path :) >> finish
        return $ DOSPlatform {
                currentTime   = currentTime,
                numCols       = numCols,
                numNoteTypes  = numNoteTypes,
                noteEvents    = noteEvents,
                columnLayout  = columnLayout,
                noteNames     = noteNames,
                eventNames    = eventNames,
                events        = events,
                headerData    = headerData,
                mergeFiles    = mergeFiles,
                userNameSpace = userNameSpace,
                namespace     = namespace,
                metaNamespace = metaNamespace
            }

