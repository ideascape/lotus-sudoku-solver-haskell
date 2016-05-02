
--[5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
--This should print out:
--[5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

--Solvable Test Cases (Let me know if you disagree):
--[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
--[4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]
--[0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]
--[4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]

--Simple test cases:
--[5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,0]

import System.Random
import Data.List
import Data.Char

ring =  zipRefs $ [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc = zipRefs $  [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc = zipRefs $  [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]

reflists = generateReflists ring clockwiseArc counterclockwiseArc

reftree = generateReftree ring clockwiseArc counterclockwiseArc [0..48]

main = do
  gen <- getStdGen
  let randoms = (randomRs ('1','7') gen)
  let solboard = []
  let solboard = filter (\(a,b) -> b/=0) (zipWith (\a b -> (a, b::Int)) [0..48] [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,0])
  let testsol = filter (\(a,b) -> b/=0) (zipWith (\a b -> (a, b::Int)) [0..48]  [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,0])
  --putStrLn $ show $ generateSolutionGroup (cleanSol testsol) ((cleanReflists reflists) !! 20)
  --putStrLn $ show $ validate $ testsol
  --putStrLn $ show $ reftree !! 48
  --putStrLn $ show $ length $ runThrough solboard reflists
  putStrLn $ debugInfo $ runThrough testsol reflists
  --putStrLn $ show $ intersectTpls (ring !! 0) [(1,0),(5,0),(66,0)]
  --let gamestate = [reflists, reftree, solboard, randoms]
  --generateSolution reflists gamestate
  return ()

generateReftree::[[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]] -> [Int] -> [[[(Int,Int)]]]
generateReftree _ _ _ [] = []
generateReftree ring cArc ccArc (index:xs) = ((filterRefs index ring) ++ (filterRefs index cArc) ++ (filterRefs index ccArc)):(generateReftree ring cArc ccArc xs)
  where filterRefs i reflists = filter (\ys -> length (intersectTpls ys [(i,0)]) == 1) reflists

generateReflists::[[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]]
generateReflists [] [] [] = []
generateReflists (ring:xs) (cArc:ys) (ccArc:zs) = ring:cArc:ccArc:(generateReflists xs ys zs)

--filterRefs::[[Int]] -> [Int]
--filterRefs [] i = []
--filterRefs (refl:xs) i = filter (\refl -> length (intersectTpls refl [(i,0)]) == 1) reflist

zipRefs::[[Int]] -> [[(Int,Int)]]
zipRefs [] = []
zipRefs reflists = zipWith (\a b -> zip a b) reflists $ replicate 7 $ replicate 7 (0 :: Int)

intersectTpls::[(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
intersectTpls left right = intersectBy (\(i,val) (index,value) -> i == index) left right

--Concatenates solboard multiple times right now. Calling function needs to nub the result. Or returns [(-1,-1)], which indicates failure.
--shaveOne::[(Int,Int)] -> [[(Int,Int)]] -> [Int] -> ([[(Int,Int)]], [(Int,Int)])
--shaveOne _ _ [] = []
--shaveOne solboard reflists (round:xs) = (fst addValResult) ++ (shaveOne (updateReflists reflists (snd addValResult) round) solboard xs)
--  where updateReflists reflists modReflist round = ((reflists \\ (reflists !! round)) ++ modReflist)
--        addValResult = addVal solboard (reflists !! round)

runThrough::[(Int,Int)] -> [[(Int,Int)]] -> [(Int,Int)]
--runThrough (elem (-1,-1) solboard) --Need to check for failed solution
runThrough solboard reflists
  | (reflists == [[(0,0)]]) = solboard
  | otherwise = runThrough (addVals solboard reflists) (shaveOne reflists)

shaveOne::[[(Int,Int)]] -> [[(Int,Int)]]
shaveOne reflists
  | (length (concat reflists) == 0) = [[(0,0)]]
  | otherwise = map (\xs -> tail xs) reflists

addVals::[(Int,Int)] -> [[(Int,Int)]] -> [(Int,Int)]
addVals solboard [] = solboard
addVals solboard reflists = (addVals (addVal solboard (head reflists)) (tail reflists))

--Returns tuple of new value for solboard
addVal::[(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
addVal solboard reflist
  | (length reflist == 0) = solboard
  | (added == (0,0)) = solboard
  | ((snd added) == -1) = added:solboard --redundant, but here for clarity
  | otherwise = added:solboard
    where added = (addAtIndex (fst $ head reflist) solboard)

--returns (index,-1) on failure, otherwise returns the validated item to place in solboard
--if index already has move on board, return empty tuple?
addAtIndex::Int -> [(Int,Int)] -> (Int,Int)
addAtIndex index solboard
  | (length intersect > 0) = (0,0)
  | (length intersect == 0) = try7 index [1..7] solboard
    where intersect = intersectTpls solboard [(index,0::Int)]

try7::Int -> [Int] -> [(Int,Int)] -> (Int,Int)
try7 index [] solboard = (index,-1) --fail condition; we checked all 7, and nothing worked. The board is wrecked, start over
try7 index (v:xs) solboard
  | (validateTpl (index,v) solboard) = (index,v)
  | otherwise = try7 index xs solboard

--Does the index already exist on the board? (Already checked by addAtIndex) If not, does the attempted value duplicate any of the other values in the node?
validateTpl::(Int,Int) -> [(Int,Int)] -> Bool
validateTpl (i,v) solboard = not (elem v (intersect (refTreeNode !! 0))) && not (elem v (intersect (refTreeNode !! 1))) && not (elem v (intersect (refTreeNode !! 2)))
  where refTreeNode = (reftree !! i)
        intersect refTreeLeaf = map (\(i,v) -> v) (intersectTpls solboard refTreeLeaf)

--Solution validation
--Takes in a reference list and generates a list of the corresponding values in the given solution
generateSolutionGroup::[Int]->[Int]->[Int]
generateSolutionGroup solution [] = []
generateSolutionGroup solution (index:xs) = (solution !! index):(generateSolutionGroup solution xs)

validateSolutionGroup::[Int] -> Bool
validateSolutionGroup solgroup
  | ((sort solgroup) == [1..7]) = True
  | otherwise = False

--Go through reflists, generate a solution group for each one, and validate each group
validateLotus::[Int] -> [[Int]] -> Bool
validateLotus solution [] = True
validateLotus solution (reflist:xs) = validateSolutionGroup (generateSolutionGroup solution reflist) && (validateLotus solution xs)

--Other input validations: are the numbers between 1 and 7; are there 7 and only 7 of each number

validate::[(Int,Int)] -> [Int]
validate solution
  | (length solution /= 49) = [-1]
  | otherwise = (if (validateLotus (cleanSol solution) (cleanReflists reflists)) == True then (cleanSol solution) else [0])

cleanReflists::[[(Int,Int)]] -> [[Int]]
cleanReflists reflists = map (\xs -> cleanReflists xs) reflists
  where cleanReflists reflist = map (\(a,b) -> a) reflist

cleanSol::[(Int,Int)] -> [Int]
cleanSol xs = map (\(a,b) -> b) xs

debugInfo::[(Int,Int)]->String
debugInfo sol = "Length: " ++ show (length sol) ++ "\n" ++ show sol ++ "\n" ++ "Validate: " ++ show (validate sol) ++ "\n" ++ "Solgroups: " ++ "\n" ++ (debugSolgroups (cleanSol sol) (cleanReflists reflists))

debugSolgroups::[Int] -> [[Int]] -> String
debugSolgroups solution [] = ""
debugSolgroups solution (reflist:xs) = show (generateSolutionGroup solution reflist) ++ show (validateSolutionGroup (generateSolutionGroup solution reflist)) ++ "\n" ++ (debugSolgroups solution xs)

--set difference operator is \\

--let arc = [(2,0),(4,0),(7,0),(9,0)]
--let board = [(2,7),(1,4)]

--intersectBy (\(i,val) (index,value) -> i == index) board arc
--zipWith (\a b -> zip a b) ring $ replicate 7 $ replicate 7 0

--zipRefs reflists = (zipRef (tail reflists)):(zipRefs (head reflists))
--  where zipRef reflist = zipWith (\a b -> zip a b) reflist $ replicate 7 $ replicate 7 (0 :: Int)
