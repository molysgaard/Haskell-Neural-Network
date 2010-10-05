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
    show a = show $ (isInput a, inputValue a, threshold a)

setInputs ::  (Graph gr) => gr Body b -> [Double] -> gr Body b
setInputs gr is = recModNode gr t t2
    where numIs = length is
          t = take numIs (nodes gr)
          t2 = map (\x -> Body {isInput = True, inputValue = x, function = id, threshold = 0}) is

recModNode ::  (Graph gr) => gr a b -> [Node] -> [a] -> gr a b
recModNode gr (n:ns) (b:bs) = recModNode (modNode gr n b) ns bs
recModNode gr _ [] = gr
recModNode gr [] _ = gr

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

modNode ::  (Graph gr) => gr a b -> Node -> a -> gr a b
modNode gr node lab
  | Just nn <- newNodes = mkGraph nn es
  | otherwise = gr
    where 
          ns = labNodes gr
          es = labEdges gr
          newNodes
            | Just _ <- lookup node ns = Just $ (node, lab) : (cutoff [] ns node)
            | otherwise = Nothing
          cutoff pre ((n, l):ns) id
            | n == id = pre ++ ns
            | otherwise = cutoff ((n, l):pre) ns id

value gr n
  | (Just (children, thisId, thisLabel, parents), gr') <- match n gr
  , True == isInput thisLabel
  = Just (inputValue thisLabel)
  | (Just (children, thisId, thisLabel, parents), gr') <- match n gr = Just $ ((function thisLabel) . sum) $ input gr parents
  | otherwise = Nothing

input gr ps = zipWith (*) (map (fromJust . (value gr) . ids) ps) (map wgs ps)
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
