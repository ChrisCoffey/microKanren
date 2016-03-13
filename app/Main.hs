module Main where

import Lib

emptyState = ([], 0)
five = callFresh (\x -> equiv x (Val "5"))
fives_ x = disj (equiv x (Val "5")) (wait $ fives_ x)
fives = callFresh fives_

fivesRev_ x = disj (wait $ fivesRev_ x) (equiv x (Val "5"))
fivesRev = callFresh fivesRev_

a_and_b = conj
          (callFresh (\a -> equiv a (Val "7")))
          (callFresh (\b -> disj (equiv b (Val "5")) (equiv b (Val "6"))))

runTest p = toOutput (p emptyState)

main :: IO ()
main = do
    print $ take 5 (runTest fives)
    print $ take 5 ( runTest fivesRev )
    print $ take 5 ( runTest a_and_b)
