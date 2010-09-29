{-# LANGUAGE PatternGuards #-}

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import System.Random
import Data.Maybe(fromJust)

data Body = Body { isInput :: Bool
                 , inputValue :: Double
                 , function :: (Double -> Double)
                 , threshold :: Double }

instance Show Body where
    show a = show $ threshold a

genNet :: Double -> (Double -> Double) -> [Node] -> IO (Gr Body Double)
genNet t f ls = do
                  weights <- randomRIOs (0.01, 0.99) (numNodes^2) :: IO [Double]
                  let edgs = edges conns weights :: [LEdge Double]
                  return $ mkGraph nodes edgs
    where conns = genConnections ls
          body  = Body { threshold = t, function = f, isInput = False, inputValue = 0 }
          numNodes = sum ls
          nodes :: [LNode Body]
          nodes = map (\x -> (x, body)) [1 .. numNodes]
          edges ((a, b):cons) (w:ws) = (a, b, w) : edges cons ws
          edges [] _ = []
          edges _ [] = []

value gr n
  | (Just (parents, thisId, thisLabel, children), gr') <- match n gr
  , True == isInput thisLabel
  = Just (inputValue thisLabel)
  | (Just (parents, thisId, thisLabel, children), gr') <- match n gr = Just $ sum $ input parents
  | otherwise = Nothing
    where input ps = zipWith (*) (map (fromJust . (value gr) . ids) ps) (map wgs ps)
          wgs (w, _) = w
          ids (_, n) = n

-- {{{ generates a random stream of type a
randomRIOs ::  (Random a) => (a, a) -> Int -> IO [a]
randomRIOs mm n = sequence $ take n (helper mm)
    where helper maxmin = (randomRIO maxmin) : (helper maxmin)
-- }}}
 

-- {{{ Generates connection in a network
genConnections :: [Int] -> [(Int, Int)]
genConnections ls = concat $ map mapup $ concat $ testh [] ls
    where mapup (a, b) = map (\x -> (a, x)) b 

testh :: [Int] -> [Int] -> [[(Int, [Int])]]
testh pls (l:t:ls) = map (\x -> (x, parents)) children : testh (l:pls) (t:ls)
    where p = sum pls
          ids = [1..(p + l + t)]
          parents = (take l $ drop p ids)-- : (testh (l:pls) ls)
          children = take t $ drop (p + l) ids-- : (testh (l:pls) ls)

testh _   (x:[])     = []
-- }}}
