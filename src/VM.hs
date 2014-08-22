{-# LANGUAGE RankNTypes #-}

module VM (
    Operation(..),
    runOperation,
    joinOperations,
    makeOperation,
    operationToFuncLiteral,
    doOperation,
    finish,
    checkTypes
) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Monoid

import Token
import VM.Data


compilerVersion :: Double
compilerVersion = 0.2


data Operation = Operation (forall p. Platform p => VMState p -> IO (Either OperationError ())) | NullOperation

instance Monoid Operation where
    mempty  = NullOperation
    mappend = joinOperations


returnL :: Monad m => a -> m (Either a b)
returnL = return . Left

returnR :: Monad m => b -> m (Either a b)
returnR = return . Right

finish :: Monad m => m (Either a ())
finish = returnR ()


runOperation :: Platform p => Operation -> VMState p -> IO (Either OperationError ())
runOperation (Operation f) state = f state
runOperation NullOperation _     = finish

joinOperations :: Operation -> Operation -> Operation
joinOperations NullOperation op            = op
joinOperations op NullOperation            = op
joinOperations (Operation f) (Operation g) = Operation $ \state -> do
        res <- f state
        case res of
            err@(Left _) -> return err
            Right _      -> g state

operationToFuncLiteral :: Operation -> Literal
operationToFuncLiteral op = case op of
        Operation f   -> FuncLiteral $ \args state ->
                checkTypes [] args $ \[] -> do
                        res <- f state
                        case res of
                            Left err -> returnL $ ArgumentError (negate 1) (show err)
                            Right _  -> finish
        NullOperation -> FuncLiteral $ \args _ ->
                checkTypes [] args $ \[] -> finish


makeOperation :: Token -> [Token] -> Operation
makeOperation f args = Operation (doOperation f args)

doOperation :: Platform p => Token -> [Token] -> VMState p -> IO (Either OperationError ())
doOperation ftoken argTokens state = do
        fval <- getValue ftoken state
        case fval of
            Left err                -> returnL $ err
            Right val@(StrValue _)  -> returnL $ OperationError (pos ftoken) (show val ++ " isn't a function")
            Right val@(NumValue _)  -> returnL $ OperationError (pos ftoken) (show val ++ " isn't a function")
            Right val@(TypeValue _) -> returnL $ OperationError (pos ftoken) (show val ++ " isn't a function")
            Right (FuncValue f)     -> do
                    -- debug
                    -- putStrLn $ show ftoken ++ "(" ++ (concat $ intersperse ", " $ map show argTokens) ++ ")"
                    isInHeader <- getIsInHeader state
                    case ftoken of
                        (Token _ idl@(MetaIdLiteral _))
                            | not isInHeader -> returnL $ OperationError (pos ftoken) ("invalid use of a header function " ++ show idl)
                            | otherwise      -> runFunc argTokens state f
                        _
                            | not isInHeader -> setIsInHeader state False >> runFunc argTokens state f
                            | otherwise      -> runFunc argTokens state f

        where
            getArgs :: Platform p => [Token] -> VMState p -> IO (Either OperationError [Value])
            getArgs tokens state = sequence' $ map (flip getValue $ state) tokens
                where
                    sequence' :: [IO (Either OperationError Value)] -> IO (Either OperationError [Value])
                    sequence' []       = returnR []
                    sequence' (x : []) = do
                        res <- x
                        case res of
                            Left err  -> returnL err
                            Right val -> returnR [val] 
                    sequence' (x : xs) = do
                        r <- x
                        case r of
                            Left err -> returnL err
                            Right v  -> do
                                rs <- sequence' xs
                                case rs of
                                    Left err -> returnL err
                                    Right vs -> returnR $ v : vs

            runFunc :: Platform p => [Token] -> VMState p -> (Platform p => [Value] -> VMState p -> IO (Either ArgumentError ())) -> IO (Either OperationError ())
            runFunc argTokens state f = do
                    args <- getArgs argTokens state
                    case args of
                        Left err   -> returnL err
                        Right args -> do
                                res <- f args state
                                case res of
                                    Left (ArgumentError index mes)
                                        | 0 <= index && index < length argTokens -> returnL $ OperationError (pos (argTokens !! index)) mes
                                        | otherwise                              -> returnL $ OperationError (pos ftoken) mes
                                    Right _                                      -> finish


getValue :: Platform p => Token -> VMState p -> IO (Either OperationError Value)
getValue (Token _ (StrLiteral str)) _               = return . Right $ StrValue str
getValue (Token _ (NumLiteral num)) _               = return . Right $ NumValue num
getValue (Token _ (FuncLiteral f)) _                = return . Right $ FuncValue f
getValue (Token _ (TypeLiteral t)) _                = return . Right $ TypeValue t
getValue (Token pos idl@(IdLiteral name)) state     = do
        value <- lookupNamespace state name
        case value of
            Just value -> returnR value
            Nothing    -> case M.lookup name defaultNamespace of
                    Just value -> returnR value
                    Nothing    -> returnL $ OperationError pos (show idl ++ " isn't defined")
getValue (Token pos idl@(MetaIdLiteral name)) state = do
        value <- lookupMetaNamespace state name
        case value of
            Just value -> returnR value
            Nothing    -> case M.lookup name defaultMetaNamespace of
                    Just value -> returnR value
                    Nothing    -> returnL $ OperationError pos (show idl ++ " isn't defined")

defaultNamespace :: M.Map String Value
defaultNamespace = M.fromList [
        ("interval" , _interval),
        ("int"      , _interval),
        ("intervalF", _intervalFrames),
        ("intF"     , _intervalFrames),
        ("bps"      , _bps),
        ("bpm"      , _bpm),
        ("tempo"    , _bpm),
        ("length"   , _length),
        ("len"      , _length),
        ("time"     , _time),
        ("frame"    , _frame),
        ("frm"      , _frame),
        ("feed"     , _feed),
        ("\\"       , _feed),
        ("back"     , _back),
        ("^"        , _back),
        ("repeat"   , _repeat),
        ("echo"     , _echo)
    ]
    where
        -- _value :: Value
        -- __value :: Platform p => [Value] -> VMState p -> IO (Either ArgumentError ())

        _interval = FuncValue __interval
        __interval args state = checkTypes [NumType] args $ \[NumValue interval] ->
            setInterval state interval >> finish

        _intervalFrames = FuncValue __intervalFrames
        __intervalFrames args state = checkTypes [NumType] args $ \[NumValue frames] ->
            do
                fps <- getFPS state
                setInterval state (frames / fps) >> finish

        _bps = FuncValue __bps
        __bps args state = checkTypes [NumType] args $ \[NumValue bps] ->
            if bps > 0.0
                then setInterval state (1.0 / bps) >> finish
                else setInterval state 0.0 >> finish

        _bpm = FuncValue __bpm
        __bpm args state = checkTypes [NumType] args $ \[NumValue bpm] ->
            if bpm > 0.0
                then setInterval state (60.0 / bpm) >> finish
                else setInterval state 0.0 >> finish

        _length = FuncValue __length
        __length args state = checkTypes [NumType] args $ \[NumValue rel] ->
            if rel > 0.0
                then setUnitLength state (4.0 / rel) >> finish
                else setUnitLength state 0.0 >> finish

        _time = FuncValue __time
        __time args state = checkTypes [NumType] args $ \[NumValue time] ->
            setCurrentTime state time >> finish

        _frame = FuncValue __frame
        __frame args state = checkTypes [NumType] args $ \[NumValue frame] ->
            do
                fps <- getFPS state
                setCurrentTime state (frame / fps) >> finish

        _feed = FuncValue __feed
        __feed args state = checkTypes [] args $ \[] ->
            do
                interval   <- getInterval state
                unitLength <- getUnitLength state
                modifyCurrentTime state (+ (interval * unitLength)) >> finish

        _back = FuncValue __back
        __back args state = checkTypes [] args $ \[] ->
            do
                interval   <- getInterval state
                unitLength <- getUnitLength state
                modifyCurrentTime state (\x -> x - (interval * unitLength)) >> finish

        _repeat = FuncValue __repeat
        __repeat args state = checkTypes [NumType, FuncType] args $ \[NumValue times, FuncValue f] ->
            do
                res <- foldr joinFuncs finish (replicate (floor times) $ f [] state)
                case res of
                    Left (ArgumentError _ mes) -> returnL $ ArgumentError 1 mes
                    Right _                    -> finish
            where
                joinFuncs :: IO (Either ArgumentError ()) -> IO (Either ArgumentError ()) -> IO (Either ArgumentError ())
                joinFuncs f g = do
                        res <- f
                        case res of
                            err@(Left _) -> return err
                            Right _      -> g

        _echo = FuncValue __echo
        __echo args _ = checkTypes [StrType] args $ \[StrValue mes] ->
            putStrLn mes >> finish

defaultMetaNamespace :: M.Map String Value
defaultMetaNamespace = M.fromList [
        ("define"     , _define),
        ("fps"        , _fps),
        ("correction" , _correction),
        ("correctionF", _correctionFrames),
        ("version"    , _version)
    ]
    where
        _define = FuncValue __define
        __define args state = checkTypes [StrType, AnyType] args $ \[StrValue name, value] ->
            defineValue state name value >> finish

        _fps = FuncValue __fps
        __fps args state = checkTypes [NumType] args $ \[NumValue fps] ->
            if fps > 0.0
                then setFPS state fps >> finish
                else returnL $ ArgumentError 0 "fps must be a positive number"

        _correction = FuncValue __correction
        __correction args state = checkTypes [NumType] args $ \[NumValue correction] ->
            setCorrection state correction >> finish

        _correctionFrames = FuncValue __correctionFrames
        __correctionFrames args state = checkTypes [NumType] args $ \[NumValue frames] ->
            do
                fps <- getFPS state
                setCorrection state (frames / fps) >> finish

        _version = FuncValue __version
        __version args _ = checkTypes [NumType] args $ \[NumValue v] ->
            if v == compilerVersion
                then finish
                else returnL $ ArgumentError 0 ("couldn't match version " ++ show v ++ " with compiler version " ++ show compilerVersion)

validateTypes :: [Type] -> [Value] -> Either ArgumentError [Value]
validateTypes types values
    | length values < length types = Left $ ArgumentError (negate 1) ("too few argument(s): require " ++ (show $ length types) ++ " arguments")
    | length values > length types = Left $ ArgumentError (negate 1) ("too many argument(s): require " ++ (show $ length types) ++ " arguments")
    | otherwise                    = zipWithM validate types (zip [0..] values)
    where
        validate :: Type -> (Int, Value) -> Either ArgumentError Value
        validate t (index, value)
            | t == AnyType      = Right value
            | typeOf value == t = Right value
            | otherwise         = Left $ ArgumentError index ("couldn't match type " ++ (show $ typeOf value) ++ " with " ++ show t)

checkTypes :: [Type] -> [Value] -> ([Value] -> IO (Either ArgumentError ())) -> IO (Either ArgumentError ())
checkTypes types values f = case validateTypes types values of
        Left err   -> returnL err
        Right args -> f args

