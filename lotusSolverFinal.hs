
--[5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
--This should print out:
--[5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

--Solvable Test Cases (Let me know if you disagree):
--[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
--[4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]
--[0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]
--[4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]

--Simple test case:
--[5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,0]

import Data.List
import Data.Char
import Data.Maybe

ring =  [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc =  [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc =  [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]

reflists = generateReflists ring clockwiseArc counterclockwiseArc
reftree = generateReftree ring clockwiseArc counterclockwiseArc [0..48]

main = do
  let testSol = [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
  let testSolA =   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
  let testSolB = [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]
  let testSolC = [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]
  let testSolD = [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]
  let testSolE = [0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0]
  let testSolF = [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]
  let testSolInvalid = [0,0,3,7,6,1,4,4,0,5,1,7,0,0,0,2,6,0,4,5,3,1,0,0,2,0,0,5,6,0,7,0,0,2,3,2,3,0,7,6,0,0,0,4,5,6,1,7,0]

  putStrLn $ show $ (generateSolution testSolA)
  --putStrLn $ show $ validatePlacement testSol 6 48


generateSolution::[Int] -> [Int]
generateSolution solboard
  | length solboard == 0 = []
  | (checkForZeros solboard) = validateSol solboard
  | otherwise = (getSol 1 `takeOne` getSol 2 `takeOne` getSol 3 `takeOne` getSol 4 `takeOne` getSol 5 `takeOne` getSol 6 `takeOne` getSol 7)
    where getSol val = generateSolution(getPlacement solboard val)
          checkForZeros solboard = length (filter (==0) solboard) == 0
          takeOne left right = if length left > 0 then left else right


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

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
