--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Writing a real application in Haskell                             --
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JSON where

--------------------------------------------------------------------------------

import Data.Text hiding (map)

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
            pure (r:rs)

quote :: Parser Char
quote = ch (=='"')

keyword :: String -> Parser String
keyword [] = pure []
keyword (x:xs) = do
    y <- ch (==x)
    ys <- keyword xs
    pure (y:ys)

-- nullP :: Parser Value
-- nullP = do
--     keyword "null"
--     pure Null

nullP :: Parser Value
nullP = keyword "null" *> pure Null

trueP :: Parser Value
trueP = keyword "true" *> pure (Bool True)

falseP :: Parser Value
falseP = keyword "false" *> pure (Bool False)

numP :: Parser Value
numP = Num <$> nat

stringP :: Parser String
stringP = between quote quote (many (ch (/='"')))

strP :: Parser Value
strP = Str <$> stringP

comma :: Parser Char
comma = ch (==',')

arrayP :: Parser Value
arrayP = Arr <$> between 
                    (ch (=='[')) 
                    (token $ ch (==']')) 
                    (valueP `sepBy` token comma)

keyValueP :: Parser (String, Value)
keyValueP = do
    key <- stringP
    token (ch (== ':'))
    val <- valueP
    pure (key,val)

objP :: Parser Value
objP = Obj <$> between (ch (=='{')) (token $ ch (=='}')) 
                (token keyValueP `sepBy` token comma)

valueP :: Parser Value
valueP = token (nullP <|> trueP <|> falseP <|> numP <|> strP <|> arrayP <|> objP)

--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Maybe a

instance FromJSON Integer where
    fromJSON (Num n) = Just n
    fromJSON _       = Nothing

instance FromJSON Int where
    fromJSON (Num n) = Just $ fromInteger n
    fromJSON _       = Nothing

instance FromJSON a => FromJSON [a] where
    fromJSON (Arr arr) = mapM fromJSON arr
    fromJSON _ = Nothing

instance {-# OVERLAPPING #-} FromJSON String where 
    fromJSON (Str xs) = Just xs
    fromJSON _ = Nothing

-- test :: FromJSON a => String -> Maybe a
-- test str = parse valueP str >>= 
--     \(val, _) -> fromJSON val

parseJSON :: FromJSON a => String -> Maybe a
parseJSON str = do
    (val, _) <- parse valueP str
    fromJSON val

--------------------------------------------------------------------------------
