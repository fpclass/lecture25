--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with IO                                                       --
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JSON where

--------------------------------------------------------------------------------

-- import Data.Text

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

strP :: Parser Value 
strP = Str <$> between quote quote (many (ch (/='"')))

keyValueP :: Parser (String, Value)
keyValueP = do 
    Str key <- strP
    token (ch (==':'))
    val <- valueP
    pure (key,val)

objP :: Parser Value 
objP = Obj <$> between (ch (=='{')) (token $ ch (=='}')) (sepBy (token keyValueP) (token $ ch (==',')))

valueP :: Parser Value 
valueP = token (nullP <|> trueP <|> falseP <|> numP <|> arrayP <|> strP <|> objP)

parseFile :: FilePath -> IO (Maybe Value)
parseFile fp = withFile fp ReadMode $ \h -> do
    str <- hGetContents h 
    pure (fst <$> parse valueP str)


--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Maybe a

instance FromJSON Int where 
    fromJSON (Num n) = Just $ fromInteger n
    fromJSON _       = Nothing

instance FromJSON a => FromJSON [a] where 
    fromJSON (Arr arr) = mapM fromJSON arr
    fromJSON _ = Nothing

instance {-# OVERLAPPING #-} FromJSON String where 
    fromJSON (Str xs) = Just xs 
    fromJSON _ = Nothing

--------------------------------------------------------------------------------
