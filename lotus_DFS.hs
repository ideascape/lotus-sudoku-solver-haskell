
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

import Data.List
import Data.Char
import Data.Maybe
import Data.Tree



ring =  [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc =  [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc =  [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]

reflists = generateReflists ring clockwiseArc counterclockwiseArc

reftree = generateReftree ring clockwiseArc counterclockwiseArc [0..48]

main = do
  let testSol =   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
  let testSol2 = [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
  --m <- newEmptyMVar
  forkIO $ do
    results <- show $ generateSolution testSol
    putStrLn results
    --return ()
  --  v <- takeMVar m
  --  putStrLn $ show v
  --threadDelay 1000000

  --putStrLn $ show $ generateSolution testSol
  --putStrLn $ show $ validatePlacement testSol 6 48
  return ()

data SolTree a = Empty | SolNode a (SolTree a) (SolTree a) (SolTree a) (SolTree a) (SolTree a) (SolTree a) (SolTree a) deriving (Show,Read,Eq)
data Status = Visited | Visiting | Unvisited

lotusSolver::[Int] -> [Int]
lotusSolver solboard = do
  generateSolution (createSolTree solboard)

generateSolution::SolNode ([Int],Status) -> SolNode ([Int],Status)
generateSolution solNode =
--validateSol needs to be changed to handle checking for zeros before doing full validation
  if length (validateSol (fst solNode)) > 0 then solNode
  else do
    setStatus solNode Visiting
      if ((getSolNodeChild 1) == Unvisited || Empty ) && (length getPlacement (fst (getSolNodeChild 1)) 1 > 0) then generateSolution solNodeChild 1 else (mark solNodeChild as Empty)
      | snd (solNodeChild 2) == Unvisited = generateSolution solNodeChild 1
      | snd (solNodeChild 3) == Unvisited = generateSolution solNodeChild 1
      | snd (solNodeChild 4) == Unvisited = generateSolution solNodeChild 1
      | snd (solNodeChild 5) == Unvisited = generateSolution solNodeChild 1
      | snd (solNodeChild 6) == Unvisited = generateSolution solNodeChild 1
      | snd (solNodeChild 7) == Unvisited = generateSolution solNodeChild 1
    setStatus SolNode Visited

getSolNodeChild::
checks to see if child exists. If not, returns empty. If it does, returns status.

createSolTree::[Int] -> SolNode ([Int],Bool)

--note: otherwise = getSol 1 ++ getSol 2 ++ getSol 3 ++ getSol 4 ++ getSol 5 ++ getSol 6 ++ getSol 7 outputs *immediately* when a solution is found, but we only want one. Not sure how to retain immediacy
generateSolution::[Int] -> [Int]
generateSolution solboard
  | length solboard == 0 = []
  | (checkForZeros solboard) = validateSol solboard
  | otherwise = getSol 1 ++ getSol 2 ++ getSol 3 ++ getSol 4 ++ getSol 5 ++ getSol 6 ++ getSol 7
--  | otherwise = returnOne [getSol 1,getSol 2,getSol 3,getSol 4,getSol 5,getSol 6,getSol 7]
    where getSol val = generateSolution(getPlacement solboard val)
          checkForZeros solboard = length (filter (==0) solboard) == 0

returnOne::[[Int]]->[Int]
returnOne sols
  | (length (valids sols) > 0) = (valids sols) !! 0
  | otherwise = []
      where valids sols = (filter (\xs -> (length xs == 49))) sols

--finds first zero in the list and attempts to add the requested value
--returns solboard unmodified if there's no zero in the list
--returns empty list if the placement is invalid
--otherwise returns modified list
getPlacement::[Int]->Int->[Int]
getPlacement solboard val
  | (findIndex (==0) solboard == Nothing) = solboard
  | (length (validatePlacement solboard val (fromJust ((findIndex (==0) solboard)))) == 0) = []
  | otherwise =
    let (x,_:ys) = splitAt (fromJust ((findIndex (==0) solboard))) solboard
    in x ++ val:ys

--will return either same solboard or empty list. Checks whether the placement conflicts with the existing solboard. Look up the index in the reftree. Check all three (is num already an elem of any reftree?). If it is, return empty list. If not, placement is validated.
validatePlacement::[Int] -> Int -> Int -> [Int]
validatePlacement solboard val index
  | ((elem val (maprtl (refTreeLeaf 0) solboard)) || (elem val (maprtl (refTreeLeaf 1) solboard)) || (elem val (maprtl (refTreeLeaf 2) solboard))) = []
  | otherwise = solboard
    where refTreeLeaf i = (reftree !! index) !! i
          maprtl refTreeLeaf solboard = map (\index -> solboard !! index) refTreeLeaf

generateReftree::[[Int]] -> [[Int]] -> [[Int]] -> [Int] -> [[[Int]]]
generateReftree _ _ _ [] = []
generateReftree ring cArc ccArc (index:xs) = ((filterRefs index ring) ++ (filterRefs index cArc) ++ (filterRefs index ccArc)):(generateReftree ring cArc ccArc xs)
  where filterRefs i reflists = filter (\ys -> elem i ys) reflists

generateReflists::[[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
generateReflists [] [] [] = []
generateReflists (ring:xs) (cArc:ys) (ccArc:zs) = ring:cArc:ccArc:(generateReflists xs ys zs)

zipRefs::[[Int]] -> [[(Int,Int)]]
zipRefs [] = []
zipRefs reflists = zipWith (\a b -> zip a b) reflists $ replicate 7 $ replicate 7 (0 :: Int)

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

--will return either same solboard or empty list, but performs different validation. Checks whether the solution is valid.
validateSol::[Int] -> [Int]
validateSol solution
  | (validateLotus solution reflists) = solution
  | otherwise = []
