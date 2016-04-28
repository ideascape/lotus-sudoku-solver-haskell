import System.Random
import Data.List

ring =  zipRefs $ [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc = zipRefs $  [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc = zipRefs $  [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]

reflists = [[0..6],[0,7,15,22,30,37,45],[0,13,20,26,33,39,46],[7..13],[1,8,16,23,31,38,46],[1,7,14,27,34,40,47],[14..20],[2,9,17,24,32,39,47],[2,8,15,21,28,41,48],[21..27],[3,10,18,25,33,40,48],[3,9,16,22,29,35,42],[28..34],[4,11,19,26,34,41,42],[4,10,17,23,30,36,43],[35..41],[5,12,20,27,28,35,43],[5,11,18,24,31,37,44],[42..48],[6,13,14,21,29,36,44],[6,12,19,25,32,38,45]]
main = do
  gen <- getStdGen
  let randoms = (randomRs ('1','7') gen)
  let solboard = []
  let reftree = generateReftree ring clockwiseArc counterclockwiseArc [0..48]
  putStrLn $ show $ reftree !! 0
  --putStrLn $ show $ intersectTpls (ring !! 0) [(1,0),(5,0),(66,0)]
  --let gamestate = [reflists, reftree, solboard, randoms]
  --generateSolution reflists gamestate
  return ()

generateReftree::[[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]] -> [Int] -> [[[(Int,Int)]]]
generateReftree _ _ _ [] = []
generateReftree ring cArc ccArc (index:xs) = ((filterRefs index ring) ++ (filterRefs index cArc) ++ (filterRefs index ccArc)):(generateReftree ring cArc ccArc xs)
  where filterRefs i reflists = filter (\ys -> length (intersectTpls ys [(i,0)]) == 1) reflists

--filterRefs::[[Int]] -> [Int]
--filterRefs [] i = []
--filterRefs (refl:xs) i = filter (\refl -> length (intersectTpls refl [(i,0)]) == 1) reflist

zipRefs::[[Int]] -> [[(Int,Int)]]
zipRefs [] = []
zipRefs reflists = zipWith (\a b -> zip a b) reflists $ replicate 7 $ replicate 7 (0 :: Int)

intersectTpls::[(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
intersectTpls left right = intersectBy (\(i,val) (index,value) -> i == index) left right

--set difference operator is \\

--let arc = [(2,0),(4,0),(7,0),(9,0)]
--let board = [(2,7),(1,4)]

--intersectBy (\(i,val) (index,value) -> i == index) board arc
--zipWith (\a b -> zip a b) ring $ replicate 7 $ replicate 7 0


--zipRefs reflists = (zipRef (tail reflists)):(zipRefs (head reflists))
--  where zipRef reflist = zipWith (\a b -> zip a b) reflist $ replicate 7 $ replicate 7 (0 :: Int)