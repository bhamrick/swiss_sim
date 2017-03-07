module Main where

import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Text.Printf
import qualified Numeric.Probability.Distribution as Dist

data WinLoss = WinLoss Int Int
    deriving (Eq, Show, Ord)

instance Monoid WinLoss where
    mempty = WinLoss 0 0
    (WinLoss w1 l1) `mappend` (WinLoss w2 l2) = WinLoss (w1+w2) (l1+l2)

win = WinLoss 1 0
loss = WinLoss 0 1

data Rankings = Rankings [(WinLoss, Int)]
    deriving (Eq, Show, Ord)

instance Monoid Rankings where
    mempty = Rankings []
    Rankings r1 `mappend` Rankings r2 = Rankings (Map.toList $ Map.unionWith (+) (Map.fromList r1) (Map.fromList r2))

simplify :: Rankings -> Rankings
simplify (Rankings r) = Rankings (filter (\(WinLoss w l, n) -> l <= 2 && n > 0) r)

advanceRound :: Fractional p => Rankings -> Dist.T p Rankings
advanceRound (Rankings r) = doPairings (reverse . sort $ r) Nothing (pure mempty)
    where
    doPairings :: Fractional p => [(WinLoss, Int)] -> Maybe WinLoss -> Dist.T p Rankings -> Dist.T p Rankings
    doPairings records pairDown acc =
        case records of
            [] -> case pairDown of
                Nothing -> acc
                Just rec -> fmap (<> Rankings [(rec <> win, 1)]) acc
            tier@(rec, count):rest -> 
                if count > 0
                then case pairDown of
                    Nothing -> if even count
                        then
                            doPairings rest Nothing $ do
                                resultsSoFar <- acc
                                let tierResults = Rankings [(rec <> win, count `div` 2), (rec <> loss, count `div` 2)]
                                pure (resultsSoFar <> tierResults)
                        else
                            doPairings rest (Just rec) $ do
                                resultsSoFar <- acc
                                let tierResults = Rankings [(rec <> win, count `div` 2), (rec <> loss, count `div` 2)]
                                pure (resultsSoFar <> tierResults)
                    Just rec' -> if even count
                        then
                            doPairings rest (Just rec) $ do
                                resultsSoFar <- acc
                                pairDownResult <- Dist.uniform
                                    [ Rankings [(rec' <> win, 1), (rec <> loss, 1)]
                                    , Rankings [(rec' <> loss, 1), (rec <> win, 1)]
                                    ]
                                let tierResults = Rankings [(rec <> win, (count - 1) `div` 2), (rec <> loss, (count - 1) `div` 2)]
                                pure (resultsSoFar <> pairDownResult <> tierResults)
                        else
                            doPairings rest Nothing $ do
                                resultsSoFar <- acc
                                pairDownResult <- Dist.uniform
                                    [ Rankings [(rec' <> win, 1), (rec <> loss, 1)]
                                    , Rankings [(rec' <> loss, 1), (rec <> win, 1)]
                                    ]
                                let tierResults = Rankings [(rec <> win, (count - 1) `div` 2), (rec <> loss, (count - 1) `div` 2)]
                                pure (resultsSoFar <> pairDownResult <> tierResults)
                else doPairings rest pairDown acc

topCut :: Int -> Rankings -> (Rankings, Int)
topCut n (Rankings r) = getCut n (reverse . sort $ r) mempty
    where
    getCut :: Int -> [(WinLoss, Int)] -> Rankings -> (Rankings, Int)
    getCut remaining records acc =
        case records of
            [] -> (acc, 0)
            tier@(rec, count):rest ->
                if count < remaining
                then getCut (remaining - count) rest (acc <> Rankings [(rec, count)])
                else (acc <> Rankings [(rec, remaining)], count - remaining)

simulate :: Fractional p => Int -> Int -> Int -> Dist.T p (Rankings, Int)
simulate numPlayers numRounds cutSize =
    Dist.norm . fmap (topCut cutSize) $ go numRounds (pure $ Rankings [(WinLoss 0 0, numPlayers)])
    where
    go :: Fractional p => Int -> Dist.T p Rankings -> Dist.T p Rankings
    go n rankingDist =
        if n <= 0
        then rankingDist
        else go (n-1) (Dist.norm . fmap simplify $ rankingDist >>= advanceRound)

pprintDist :: (Show a) => Dist.T Double a -> IO ()
pprintDist dist = do
    for_ (Dist.decons dist) $ \(a, p) -> do
        printf "%5.2f%%\t%s\n" (100*p) (show a)

main :: IO ()
main = do
    pprintDist $ simulate 226 8 8
