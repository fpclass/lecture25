--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 25: Fun with IO                                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module JSON where

--------------------------------------------------------------------------------

import Data.Text

import System.IO hiding (hGetContents)
import System.IO.Strict

import Parser

--------------------------------------------------------------------------------
-- JSON representation

type Object = [(String, Value)]
type Array  = [Value]

data Value = Obj Object
           | Arr Array
           | Str String
           | Num Integer
           | Bool Bool
           | Null
           deriving Show

--------------------------------------------------------------------------------
-- JSON parsing

-- Reminder:
-- instance Functor Parser
-- instance Applicative Parser
-- instance Monad Parser

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = go <|> pure []
    where go = do
            r <- p
            rs <- many (sep *> p)
            return (r:rs)

nullP :: Parser Value
nullP = do
    keyword "null"
    return Null

trueP :: Parser Value
trueP = do
    keyword "true"
    return $ Bool True

falseP :: Parser Value
falseP = do
    keyword "false"
    return $ Bool False

boolP :: Parser Value
boolP = trueP <|> falseP

natP :: Parser Value
natP = Num <$> nat

strP :: Parser Value
strP = Str <$> between (ch (=='"')) (ch (=='"')) (many (ch (/='"')))

arrP :: Parser Value
arrP = Arr <$> between (ch (=='[')) (token $ ch (==']')) (sepBy valP (token $ ch (==',')))

keyValueP :: Parser (String, Value)
keyValueP = do
    Str key <- strP
    token (ch (==':'))
    val <- valP
    return (key, val)

objP :: Parser Value
objP = Obj <$> between (ch (=='{')) (token $ ch (=='}')) (sepBy (token keyValueP) (token $ ch (==',')))

valP :: Parser Value
valP = token (nullP <|> boolP <|> natP <|> strP <|> arrP <|> objP)

-- | `parseFile` @filepath@ parses a JSON document located at @filepath@.
parseFile :: FilePath -> IO (Maybe Value)
parseFile fp = withFile fp ReadMode $ \h -> do
    xs <- hGetContents h
    return (fst <$> parse valP xs)

--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Maybe a



--------------------------------------------------------------------------------
