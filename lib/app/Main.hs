module Main where

-- ghcid command:
-- ghcid --command="stack ghci" --restart=package.yaml

import Control.Dashboard
import Data.List (sortBy)
import Data.Ord

main :: IO ()
main = do
    (nonzero, allPrs) <- nonzeroPrs Nothing "hipchat"

    putStrLn "All the repos that have some PRs!"
    print nonzero

    putStrLn "5 most recent!"
    print $ (take 5 $ sortBy (\(_,_,a) (_,_,b) -> compare (Down a) (Down b)) allPrs)

    putStrLn "Outputting the graph!"
    makePrGraph "nonzero_prs.png" nonzero
