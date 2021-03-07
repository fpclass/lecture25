--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Writing a real application in Haskell                             --
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



--------------------------------------------------------------------------------

class FromJSON a where
    fromJSON :: Value -> Maybe a



--------------------------------------------------------------------------------
