{-# LANGUAGE OverloadedStrings #-}

module Folds.Folds where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void (Void)

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember = CrewMember
  { _name :: String
  , _role :: Role
  , _talents :: [String]
  }
  deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster :: S.Set CrewMember
roster =
  S.fromList
    -- Name      Name               Role         Talents
    [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"]
    , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
    , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"]
    , CrewMember "One-eyed Jack" Navigator []
    ]

-- Can't set and no polymorphic actions, hence simpler type signature
myFold :: Fold s a
myFold = undefined

-- We can focus _zero or more_ Roles if given a Set of CrewMembers
-- N.B. Role not [Role] as all folds return a list (actually a Foldable)
rosterRoles :: Fold (S.Set CrewMember) Role
rosterRoles = undefined

-- We'll need to build and action like this via composition:
-- toListSomehow :: Fold (S.Set CrewMember) Role -> S.Set CrewMember -> [Role]

-- This is unneeded really, as folded is just fine used inline
crewMembers :: Foldable f => Fold (f CrewMember) CrewMember
crewMembers = folded

-- Note that we use ^.. (toListOf) because ^. (view) will (<>) the items.
-- This is non-intuitive so foldOf is available for this kind of thing.
-- toListOf always returns a list and cannot return arbitrary Foldables
crewMembersList :: [CrewMember]
crewMembersList = roster ^.. crewMembers

-- Folding on other Foldables

foldPair :: Bool
foldPair = (("foo", 42) ^.. folded) == [42]

foldedMayReturnEmpty :: Bool
foldedMayReturnEmpty = null $ (Nothing :: Maybe Void) ^.. folded

foldingMapOnlyFocusesValues :: Bool
foldingMapOnlyFocusesValues = (newMap ^.. folded) == ["Captain", "First Mate"]
 where
  newMap = M.fromList [("Jack", "Captain"), ("Will", "First Mate")]

-- Using lenses as folds

-- We don't need to do this, but it demonstrates the lenses are valid folds
-- Lens' s a becomes Fold s a (do polymorphic lenses work still then?)
crewRole :: Fold CrewMember Role
crewRole = role

-- Note that we lost ordering guarantees due to Set
focusAllRoles :: [Role]
focusAllRoles = roster ^.. folded . role

foldsCanBeUsedOnNonFoldableValues :: [Role]
foldsCanBeUsedOnNonFoldableValues = jerry ^.. role
 where
  jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]

-- Note that Folds are valid Traversals also:
-- both :: Bitraversable r => Traversal (r a a) (r b b) a b
-- both :: Bitraversable r => Fold (r a a) a
-- (More on Traversals later)
-- A Bitraversable structure is like a Traversable structure,
-- but it can be Traversed over two different type parameters!
-- The simplest types like this are tuples: (a, b), and eithers: (Either a b)

-- The last two types in a tuple are Bitraversable
usingBothOnTuple :: Bool
usingBothOnTuple = (("Gemini", "Leo", "Libra") ^.. both) == ["Leo", "Libra"]

usingBothOnEither :: Bool
usingBothOnEither =
  (Left "Albuquerque" ^.. both) == ["Albuquerque"]
    && (Right "Yosimite" ^.. both) == ["Yosimite"]

eachAllowsTurningTupleToList :: Bool
eachAllowsTurningTupleToList = ((1, 2, 3, 4, 5) ^.. each) == [1 .. 5]

-- Ex 1

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

oneFolded :: Bool
oneFolded = (beastSizes ^.. folded) == beastSizes

twoFolded :: Bool
twoFolded = (beastSizes ^.. folded . folded) == ["Sirens", "Kraken", "Ogopogo"]

threeFolded :: Bool
threeFolded = (beastSizes ^.. folded . folded . folded) == "SirensKrakenOgopogo"

threeFolded' :: Bool
threeFolded' = (beastSizes ^. folded . folded) == "SirensKrakenOgopogo"

foldAndFocus :: Bool
foldAndFocus = (beastSizes ^.. folded . _2) == ["Sirens", "Kraken", "Ogopogo"]

concat' :: Bool
concat' = toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]] == [1 .. 6]

smushed :: Bool
smushed = toListOf (folded . folded) newMap == "CaptainFirst Mate"
 where
  newMap :: M.Map String String
  newMap = M.fromList [("Jack", "Captain"), ("Will", "First Mate")]

bothThenFolded :: Bool
bothThenFolded = (tuple ^.. both . folded) == "HelloIt's me"
 where
  tuple :: (String, String)
  tuple = ("Hello", "It's me")

whySoSerious :: Bool
whySoSerious = (("Why", "So", "Serious?") ^.. each) == ["Why", "So", "Serious?"]

tripleEach :: Bool
tripleEach = (quotes ^.. each . each . each) == "WhySoSerious?ThisisSPARTA"
 where
  quotes :: [(T.Text, T.Text, T.Text)]
  quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

-- Ex 2

-- folded :: Fold [(Int, Char)] (Int, Char)
-- _1 :: Fold (Int, Char) Int
--toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]

-- folded :: Fold (S.Set String) String
-- _2 :: Fold (Bool, S.Set String) (S.Set String)
-- toListOf
--   :: (Fold (Bool, S.Set String) String)
--   -> (Bool, S.Set String)
--   -> [String]
-- toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])

-- folded_1 ::  Fold (M.Map String String) String
-- folded_2 ::  Fold [String] String
-- toListOf
--   :: (Fold (M.Map String String) String)
--   -> (M.Map String String)
--   -> String
-- toListOf (folded . folded)
--   (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
--   "CaptainFirst Mate"

-- Ex 3

foldedList :: Bool
foldedList = ([1 .. 3] ^.. folded) == [1 .. 3]

foldedTuple :: Bool
foldedTuple = (("Light", "Dark") ^.. folded) == ["Dark"]

foldedTupleList :: Bool
foldedTupleList =
  ([("Light", "Dark"), ("Happy", "Sad")] ^.. folded . both)
    == ["Light", "Dark", "Happy", "Sad"]

focussedTupleList :: Bool
focussedTupleList =
  ([("Light", "Dark"), ("Happy", "Sad")] ^.. each . _1)
    == ["Light", "Happy"]

-- TODO: This revealed a lack of understand of each vs folded
-- TODO: Maybe it will be clear with the Traversals chapter
-- TODO: given that each is a Traversal
smushedTupleList :: Bool
smushedTupleList =
  (tupleList ^.. each . _2 . folded) == "DarkSad"
    && (tupleList ^. each . _2) == "DarkSad"
 where
  tupleList :: [(String, String)]
  tupleList = [("Light", "Dark"), ("Happy", "Sad")]

basicEach :: Bool
basicEach = (("Bond", "James", "Bond") ^.. each) == ["Bond", "James", "Bond"]
