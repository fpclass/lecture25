--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 25: Fun with IO                                                    --
--------------------------------------------------------------------------------

module Parser where

import Data.Char

--------------------------------------------------------------------------------
-- Parsers

data Parser a = MkParser (String -> Maybe (a, String))

-- | Runs a parser on some input. If successful, the result of the parser
-- is returned along with any remaining input.
parse :: Parser a -> String -> Maybe (a, String)
parse (MkParser f) xs = f xs

-- | A function which, given a predicate, constructs a parser that succeeds
-- if the first character in the input satisfies the predicate.
ch :: (Char -> Bool) -> Parser Char
ch p = MkParser $ \xs -> case xs of
    (y:ys) | p y -> Just (y, ys)
    _            -> Nothing

--------------------------------------------------------------------------------
-- Parsers are functors

instance Functor Parser where
    fmap f (MkParser g) =
        MkParser $ \xs -> fmap (\(x,ys) -> (f x, ys)) (g xs)

--------------------------------------------------------------------------------
-- Parsers are applicative functors

instance Applicative Parser where
    pure x = MkParser $ \xs -> Just (x, xs)

    (MkParser a) <*> p = MkParser $ \xs -> case a xs of
        Nothing      -> Nothing
        Just (f, ys) -> let (MkParser b) = p in case b ys of
            Nothing      -> Nothing
            Just (x, zs) -> Just (f x, zs)

--------------------------------------------------------------------------------
-- Parsers are monads

instance Monad Parser where
    MkParser a >>= f = MkParser $ \xs -> case a xs of
        Nothing     -> Nothing
        Just (x,ys) -> let (MkParser b) = f x in b ys

--------------------------------------------------------------------------------
-- Alternative

infixl 3 <|>
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some  :: f a -> f [a]
    some p = (:) <$> p <*> many p

    many  :: f a -> f [a]
    many p = some p <|> pure []

instance Alternative Parser where
    empty = MkParser (const Nothing)

    (MkParser a) <|> (MkParser b) =
        MkParser $ \xs -> case a xs of
            Just r  -> Just r
            Nothing -> b xs

    many (MkParser p) = MkParser go
        where go xs = case p xs of
                Nothing     -> Just ([],xs)
                Just (r,ys) -> let Just (rs,zs) = go ys
                               in Just (r:rs, zs)

--------------------------------------------------------------------------------

-- | `nat` is a parser for natural numbers.
nat :: Parser Integer
nat = read <$> some (ch isDigit)

-- | `choice` @ps@ tries parsers from @ps@ in order.
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | `oneOf` @xs@ parses one character specified in @xs@.
oneOf :: [Char] -> Parser Char
oneOf = choice . map (ch . (==))

-- | `whitespace` is a parser for whitespace characters.
whitespace :: Parser String
whitespace = many (oneOf [' ', '\t', '\n', '\r'])

-- | `token` @p@ parses zero or more whitespace characters followed by @p@.
token :: Parser a -> Parser a
token p = whitespace *> p

-- | `between` @open close p@ first parses @open@ and discards the result,
-- then parses @p@ whose result is returned, and finally parses @close@
-- whose result is also discarded.
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | `keyword` @string@ parses @string@.
keyword :: String -> Parser String
keyword [] = return []
keyword (x:xs) = do
    y <- ch (==x)
    ys <- keyword xs
    return (y:ys)

--------------------------------------------------------------------------------
