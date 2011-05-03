{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import System.Random
import Data.Maybe(fromJust)
import qualified Data.Map as M

-- {{{ Body
-- | the datatype that holds the important stuff in a node
data Body = Body { function :: (Double -> Double)
                 , threshold :: Double }

instance Show Body where
    show a = show $ threshold a
-- }}}

type Put = M.Map Node Double

{-- | set the inputs of a net
setInputs ::  (Graph gr) => gr Body b -> [Double] -> gr Body b
setInputs gr is
  | length t /= length is = error "Number of input values and input nodes must match."
  | otherwise = recModNode gr t t2
    where t = findInputs gr
          t2 = map (\x -> Body {isInput = True, inputValue = x, function = id, threshold = 0}) is
--}

-- | modify multiple nodes
recModNode ::  (Graph gr) => gr a b -> [Node] -> [a] -> gr a b
recModNode gr (n:ns) (b:bs) = recModNode (modNode gr n b) ns bs
recModNode gr _ [] = gr
recModNode gr [] _ = gr

-- | generate a network
genNet :: Double -> (Double -> Double) -> [Node] -> IO (Gr Body Double)
genNet t f ls = do
                  weights <- randomRIOs (0.01, 0.99) (numNodes^2) :: IO [Double]
                  let edgs = edges conns weights :: [LEdge Double]
                  return $ mkGraph nodes edgs
    where conns = genConnections ls
          body  = Body { threshold = t, function = f}
          numNodes = sum ls
          nodes :: [LNode Body]
          nodes = map (\x -> (x, body)) [1 .. numNodes]
          edges ((a, b):cons) (w:ws) = (a, b, w) : edges cons ws
          edges [] _ = []
          edges _ [] = []

-- modify a node in a net, returns the new net
modNode :: (Graph gr) => gr a b -> Node -> a -> gr a b
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

-- modify a node in a net, returns the new net
modWeight gr from to b
  | Just nes <- newEdges = mkGraph ns nes
  | otherwise = gr
    where 
          ns = labNodes gr
          es = labEdges gr
          newEdges = mod' from to b [] es

-- | modifies one weight in a list of weights
mod' :: (Eq a, Eq a1) =>a -> a1 -> t -> [(a, a1, t)] -> [(a, a1, t)] -> Maybe [(a, a1, t)]
mod' a b n pre (nod@(x, y, z):xs)
  | (a == x) && (b == y) = Just $ (x, y, n) : (pre ++ xs)
  | otherwise = mod' a b n (nod:pre) xs
mod' _ _ _ _ [] = Nothing

modAdj :: Node -> b -> [(b, Node)] -> [(b, Node)] -> Maybe [(b, Node)]
modAdj nodeNum newWeight pre (n@(weight, from):xs)
  | (nodeNum == from) = Just $ (newWeight, from) : (pre ++ xs)
  | otherwise = modAdj nodeNum newWeight (n:pre) xs
modAdj _ _ _ [] = Nothing


-- | computes the value of node n in the net
value ::  (Graph gr) => gr Body Double -> Put -> Node -> Maybe Double
value gr i n
  | (Just (parents, thisId, thisLabel, children), gr') <- match n gr
  , n `M.member` i
  = M.lookup n i
  | (Just (parents, thisId, thisLabel, children), gr') <- match n gr = Just $ ((function thisLabel) . sum) $ input gr parents
  | otherwise = Nothing
    where input gr ps = zipWith (*) (map (fromJust . (value gr i) . ids) ps) (map wgs ps)
          wgs (w, _) = w
          ids (_, n) = n

-- | computes the value of all the nodes in the net.
mapVal ::  (Graph gr) => gr Body Double -> Put -> M.Map Node Double
mapVal gr input = M.fromAscList $ zip ns $ map (fromJust . (value gr input)) ns
    where ns = nodes gr

{-- | finds the input nodes
findInputs ::  (Graph gr) => gr a b -> [Node]
findInputs gr = map snd $ filter (\(a,_) -> a ==0) (zip (map (indeg gr) (nodes gr)) (nodes gr))

findOtputs = findInputs . grev
--}

-- TODO DENNE ER FEIL (kanskje ikke mer)
genDelta :: (DynGraph gr) =>gr Body Double -> (Put, M.Map Node Double) -> M.Map Node Double
genDelta gr (inputs, outs) = deltas
    where propNet = mapVal gr inputs
          propOutputDelta = M.intersectionWith (-) outs propNet
          backPropNet = mapVal (grev gr) propOutputDelta
          deltas = M.intersectionWith (-) propNet backPropNet
          --deltas = M.intersectionWith (-) propNet backPropNet

genNewWeigths :: (DynGraph gr) =>gr Body Double-> Double-> (Put, M.Map Node Double)-> gr Body Double
genNewWeigths gr ratio io = gmap f gr
    where deltas = genDelta gr io
          f (to, this, lab, from) = (to , this, lab, map (t this) from)
          t this (w, n) = (w - (((* ratio) . fromJust) $ M.lookup this deltas), n)

trainSample :: (DynGraph gr) =>gr Body Double-> Double-> Double-> (Put, M.Map Node Double)-> gr Body Double
trainSample gr ratio alpha io
  | alpha < calcerror newNet io = trainSample newNet ratio alpha io
  | otherwise = newNet
    where newNet = genNewWeigths gr ratio io

inp = M.fromList [(1,1)] :: Put
out = M.fromList [(2,1)] :: Put
tiss = (inp,Main.out)

{--
tissetass :: (DynGraph gr) =>gr Body Double -> Double -> (Put, Put) -> gr Body Double
tissetass gr ratio (inputs, outs) = gmap f gr
    where grads = genDelta gr (inputs, outs)
          --f (to, this, lab, from) = (fromJust (modAdj this (fromJust $ lookup this grads) [] to), this, lab, from)
          f (to, this, lab, from) = (mapses this to, this, lab, from)
          mapses this ((b,i):xs) = (b - ((fromJust $ value gr i) * (fromJust $ M.lookup this grads) * ratio), i) : mapses this xs
          mapses _ [] = []

iterate gr ratio io alpha = foldr (\a b -> trainSample b ratio a alpha) gr io

trainSample :: (DynGraph gr) =>gr Body Double-> Double-> (Put, Put) -> Double-> gr Body Double
trainSample gr ratio io alpha
  | calcerror gr io > alpha = trainSample (tissetass gr ratio io) ratio io alpha
  | otherwise = gr

trainNTimes :: (DynGraph gr) =>gr Body Double-> Double-> (Put, Put) -> Int -> gr Body Double
trainNTimes gr ratio io 0 = gr
trainNTimes gr ratio io n = trainNTimes (tissetass gr ratio io) ratio io (n - 1)
--}

-- TODO sjekke om denne stemmer
calcerror :: (DynGraph gr) =>gr Body Double -> (Put, Put) -> Double
calcerror gr io@(i, o) = sqrt . sum $ map (^2) $ M.elems $ M.filterWithKey (\a _ -> M.member a o) $ genDelta gr io



{-- zips a map with a function
mapMaps :: (Eq t) => (t1 -> a -> t2) -> [(t, t1)] -> [(t, a)] -> [(t, t2)]
mapMaps f ((a,x):xs) ys = (a, (f x ((fromJust . (lookup a)) ys))) : mapMaps f xs ys
mapMaps _ [] _ = []--}


-- {{{ generates a random stream of type a with n units
randomRIOs ::  (Random a) => (a, a) -> Int -> IO [a]
randomRIOs mm n = sequence $ take n (helper mm)
    where helper maxmin = (randomRIO maxmin) : (helper maxmin)
-- }}}
 

-- {{{ Generates connection in a network
genConnections :: [Int] -> [(Int, Int)]
genConnections ls = concat $ map mapup $ concat $ testh [] ls
    where mapup (a, b) = map (\x -> (a, x)) b 

testh :: [Int] -> [Int] -> [[(Int, [Int])]]
testh pls (l:t:ls) = map (\x -> (x, children)) parents : testh (l:pls) (t:ls)
    where p = sum pls
          ids = [1..(p + l + t)]
          parents = (take l $ drop p ids)-- : (testh (l:pls) ls)
          children = take t $ drop (p + l) ids-- : (testh (l:pls) ls)

testh _   (x:[])     = []
-- }}}

e :: Double
e = 2.71828182845904523536

sigmoid :: Double -> Double
sigmoid t = 1 / (1 + (e ** (- t)))

test = genNet 0.0 sigmoid [2,4,1]
