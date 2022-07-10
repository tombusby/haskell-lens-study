{-# LANGUAGE RecordWildCards #-}

module Folds.CustomFolds where

import Control.Lens
import qualified Data.Set as S

newtype Name = Name
  { getName :: String
  }
  deriving (Show, Eq)

data ShipCrew = ShipCrew
  { _shipName :: Name
  , _captain :: Name
  , _firstMate :: Name
  , _conscripts :: [Name]
  }
  deriving (Show)

makeLenses ''ShipCrew

crewMembers :: Fold ShipCrew Name
crewMembers = folding $ \ShipCrew{..} -> [_captain, _firstMate] ++ _conscripts

-- Alternative version using folds
crewMembers' :: Fold ShipCrew Name
crewMembers' = folding $ \s ->
  s ^.. captain
    <> s ^.. firstMate
    <> s ^.. conscripts . folded

myCrew :: ShipCrew
myCrew =
  ShipCrew
    { _shipName = Name "Purple Pearl"
    , _captain = Name "Grumpy Roger"
    , _firstMate = Name "Long-John Bronze"
    , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
    }

getCrewMembers :: Bool
getCrewMembers =
  myCrew ^.. crewMembers
    == [ Name{getName = "Grumpy Roger"}
       , Name{getName = "Long-John Bronze"}
       , Name{getName = "One-eyed Jack"}
       , Name{getName = "Filthy Frank"}
       ]

mapUsingTo :: Bool
mapUsingTo =
  myCrew ^.. crewMembers . to getName
    == ["Grumpy Roger", "Long-John Bronze", "One-eyed Jack", "Filthy Frank"]

-- Combining Folds

crewNames :: Fold ShipCrew Name
crewNames = folding $ \s ->
  s ^.. captain <> s ^.. firstMate <> s ^.. conscripts . folded

mapUsingTo' :: Bool
mapUsingTo' =
  myCrew ^.. crewNames . to getName
    == ["Grumpy Roger", "Long-John Bronze", "One-eyed Jack", "Filthy Frank"]

-- Ex 1

wizardHarry :: Bool
wizardHarry =
  ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
    == "YerawizardHarry"

dropConcat :: Bool
dropConcat =
  [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
    == [1, 2, 4, 5]

dropFromAllSubLists :: Bool
dropFromAllSubLists =
  [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
    == [[1, 2], [4, 5]]

palindromes :: Bool
palindromes =
  ["bob", "otto", "hannah"] ^.. folded . to reverse
    == ["bob", "otto", "hannah"]

tuplesToString :: Bool
tuplesToString =
  ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded
    == "cbafed"

-- Ex 2

multiplyNums :: Bool
multiplyNums = [1 .. 5] ^.. folded . to (* 100) == [100, 200 .. 500]

tupleToList :: Bool
tupleToList = (1, 2) ^.. each == [1, 2]

listOfTuplesOfLists :: Bool
listOfTuplesOfLists =
  [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded == [1 .. 8]

leftRight :: Bool
leftRight =
  let rightToEven n = if even n then Right n else Left n
   in [1, 2, 3, 4] ^.. folded . to rightToEven
        == [Left 1, Right 2, Left 3, Right 4]

listOfNestedTuples :: Bool
listOfNestedTuples =
  [(1, (2, 3)), (4, (5, 6))] ^.. folded . (_1 <> _2 . each)
    == [1 .. 6]

interspersedEither :: Bool
interspersedEither =
  [(1, "one"), (2, "two")] ^.. folded . (_1 . to Left <> _2 . to Right)
    == [Left 1, Right "one", Left 2, Right "two"]

reverseAndFold :: Bool
reverseAndFold =
  S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
    == "selppastocirpa"

-- Ex 3

showReverseAndConcat :: Bool
showReverseAndConcat =
  [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . to reverse . folded
    == "54321"

evenIndexTuples :: Bool
evenIndexTuples =
  let tupleList = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
      selectTuple (n, l) = [l | even n]
   in tupleList ^.. folded . folding selectTuple
        == ["b", "d"]
