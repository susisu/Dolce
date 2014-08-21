{-# LANGUAGE RankNTypes #-}

module Token (
    Literal(..),
    Token(..)
) where

import Text.Parsec.Pos (SourcePos)

import VM.Data


data Literal =
      IdLiteral String
    | MetaIdLiteral String
    | StrLiteral String
    | NumLiteral Double
    | FuncLiteral (forall p. Platform p => [Value] -> VMState p -> IO (Either ArgumentError ()))
    | TypeLiteral Type

instance Show Literal where
    show (IdLiteral name)     = name
    show (MetaIdLiteral name) = "#" ++ name
    show (StrLiteral str)     = "\"" ++ str ++ "\""
    show (NumLiteral num)     = show num
    show (FuncLiteral _)      = "[Function]"
    show (TypeLiteral t)      = show t

data Token = Token {pos :: SourcePos, lit :: Literal}

instance Show Token where
    show (Token p l) = show p ++ " : " ++ show l

