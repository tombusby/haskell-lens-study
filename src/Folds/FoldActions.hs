module Folds.FoldActions where

import Control.Lens

-- Ex 1

q1_1 :: Bool
q1_1 = not $ has folded []

q1_2 :: Bool
q1_2 = foldOf both ("Yo", "Adrian!") == "YoAdrian!"

q1_3 :: Bool
q1_3 = elemOf each "phone" ("E.T.", "phone", "home")

q1_4 :: Bool
q1_4 = minimumOf folded [5, 7, 2, 3, 13, 17, 11] == Just 2

q1_5 :: Bool
q1_5 = lastOf folded [5, 7, 2, 3, 13, 17, 11] == Just 11

q1_6 :: Bool
q1_6 = anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]

q1_7 :: Bool
q1_7 = findOf folded even [11, 22, 3, 5, 6] == Just 22

-- Ex 2

q2_1 :: Bool
q2_1 =
  findOf
    folded
    (\s -> reverse s == s)
    [ "umbrella"
    , "olives"
    , "racecar"
    , "hammer"
    ]
    == Just "racecar"

q2_2 :: Bool
q2_2 = anyOf each even (2, 4, 6)

q2_3 :: Bool
q2_3 =
  foldrOf
    folded
    foldFunc
    Nothing
    [(2, "I'll"), (3, "Be"), (1, "Back")]
    == Just (3, "Be")
 where
  foldFunc :: Ord a => a -> Maybe a -> Maybe a
  foldFunc t1 Nothing = Just t1
  foldFunc t1 (Just t2) = Just $ if t1 > t2 then t1 else t2

-- Ex 3

q3_2 :: Bool
q3_2 =
  foldrOf (folded . folded) (\x xs -> xs ++ [x]) [] ["a", "b", "c"] == "cba"
