--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with IO                                                       --
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

quote :: Parser Char 
quote = ch (=='"')

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = go <|> pure []
    where go = do 
            r <- p
            rs <- many (sep *> p)
            pure (r:rs)

arrayP :: Parser Value 
arrayP = Arr <$> between (ch (=='[')) (token $ ch (==']')) 
                    (sepBy valueP (token $ ch (==',')))

nullP :: Parser Value 
nullP = do 
    keyword "null"
    pure Null

trueP :: Parser Value 
trueP = do 
    keyword "true"
    pure (Bool True)   

falseP :: Parser Value 
falseP = do 
    keyword "false"
    pure (Bool False)

numP :: Parser Value 
numP = Num <$> nat

valueP :: Parser Value 
valueP = token (nullP <|> trueP <|> falseP <|> numP <|> arrayP)

--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Maybe a



--------------------------------------------------------------------------------
