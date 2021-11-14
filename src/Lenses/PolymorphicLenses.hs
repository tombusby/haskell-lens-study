module Lenses.PolymorphicLenses where

import Control.Lens

data Preferences a = Preferences
  { _best :: a,
    _worst :: a
  }
  deriving (Show)

best :: Lens (Preferences a) (Preferences b) a b
best = lens getter setter
  where
    getter = _best
    -- We can't make a setter here since _best and _worst have the same
    -- type variable. We'll need a Traversal to do that.
    setter = undefined

-- Q1

data Vorpal a

vorpal :: Lens (Vorpal a) (Vorpal b) a b
vorpal = lens getter setter
  where
    getter = undefined
    setter = undefined

-- Q3 / Q4

-- Making the msg type polymorphic is the solution to Q4
data Result e msg = Result
  { _lineNumber :: Int,
    _result :: Either e msg
  }

lineNumber :: Lens' (Result a1 a2) Int
lineNumber = lens getter setter
  where
    getter = _lineNumber
    setter resultObj newVal = resultObj {_lineNumber = newVal}

result :: Lens (Result a1 a2) (Result b1 b2) (Either a1 a2) (Either b1 b2)
result = lens getter undefined
  where
    getter = _result
    setter resultObj newVal = resultObj {_result = newVal}

-- Q5

data Predicate a = Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter (Predicate p) = p
    setter _ newP = Predicate newP
