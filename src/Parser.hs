{-
    Dolce - Parser.hs
    Copyright (c) 2014, Susisu
    see: license.txt
-}

module Parser (
    parse
) where

import Control.Applicative hiding ((<|>))
import Data.Char
import Data.Monoid
import Numeric
import qualified Text.Parsec as P
import Text.Parsec.Prim ((<|>), (<?>))

import Token
import VM
import VM.Data


type Parser = P.Parsec String ()


infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)


parse :: String -> String -> Either P.ParseError Operation
parse name source = P.parse (dol <* P.eof) name source


dol :: Parser Operation
dol = skips *> (mconcat <$> (line <|> (skips *> pure mempty)) `P.sepBy` separator)

line :: Parser Operation
line = mconcat <$> (reverse <$> (operation <|> (skips *> pure mempty)) `P.sepBy` reverser)

operation :: Parser Operation
operation = makeOperation <$> (token <* skips) <*> P.many (token <* skips)


separator :: Parser ()
separator = skips *> ((P.char ';' *> pure ()) <|> eol) <* skips

reverser :: Parser ()
reverser = skips *> (P.char ':' *> pure ()) <* skips


skips :: Parser ()
skips = P.skipMany (comment <|> whiteSpace)

eol :: Parser ()
eol = P.oneOf "\n\r\v" *> pure () <?> "end of line"

comment :: Parser ()
comment = P.try (P.string "--") *> P.manyTill P.anyChar (P.try . P.lookAhead $ eol *> pure () <|> P.eof) *> pure () <?> "comment"

whiteSpace :: Parser ()
whiteSpace = P.oneOf " \f\t" *> pure () <?> "white space"


token :: Parser Token
token = Token <$> P.getPosition <*> literal

literal :: Parser Literal
literal = idLiteral <|> metaIdLiteral <|> strLiteral <|> numLiteral <|> typeLiteral <|> funcLiteral

idName :: Parser String
idName = P.noneOf " \f\n\r\t\v;:#%'`(){}[]\".,+-0123456789" <:> (P.many $ P.noneOf " \f\n\r\t\v;:#%'`(){}[]\".,+-") <?> "id name"

idLiteral :: Parser Literal
idLiteral = IdLiteral <$> idName <?> "identifier"

metaIdLiteral :: Parser Literal
metaIdLiteral = MetaIdLiteral <$> (P.char '#' *> idName) <?> "metaIdentifier"

strLiteral :: Parser Literal
strLiteral = StrLiteral <$> (foldr (:) "" <$> (P.char '\"' *> P.many (P.char '\\' *> escaped <|> P.noneOf "\\\"\f\n\r\v") <* P.char '\"')) <?> "string"
    where
        escaped :: Parser Char
        escaped = unescapeUnicode <$> (P.char 'u' *> P.count 4 P.hexDigit) <|> unescapeChar <$> P.noneOf "u\f\n\r\v" <?> "escaped character"

        unescapeChar :: Char -> Char
        unescapeChar char = case char of
                'b' -> '\b'
                'f' -> '\f'
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                'v' -> '\v'
                c   -> c

        unescapeUnicode :: String -> Char
        unescapeUnicode = chr . fst . head . readHex

numLiteral :: Parser Literal
numLiteral = NumLiteral <$> float <?> "number"
    where
        float :: Parser Double
        float = read <$>
                 (P.option '0' (P.char '-') <:> P.many1 P.digit)
            <++> P.option "" (P.char '.' <:> P.many1 P.digit)
            <++> P.option "" (P.oneOf "Ee" <:> P.option '+' (P.oneOf "+-") <:> P.many1 P.digit)

typeLiteral :: Parser Literal
typeLiteral = P.char '%' *> (TypeLiteral <$> rawType) <* P.char '%' <?> "type"
    where
        rawType = (P.string "String" *> pure StrType)
            <|> (P.string "Number" *> pure NumType)
            <|> (P.string "Function" *> pure FuncType)
            <|> (P.string "Type" *> pure TypeType)

funcLiteral :: Parser Literal
funcLiteral = P.char '{' *> (operationToFuncLiteral <$> dol) <* P.char '}'
        

