import Data.List
import Data.Maybe

-- *** General Comments *** --

--I would describe the process of learning Haskell, and this project in particular, as among the most difficult programming tasks I have attempted. I am not ashamed to admit I went through four different approaches in the course of completing this project. There is no question that it has been time well spent - the level of comfort achieved with recursion, lambda functions, list comprehensions, and even some data structures will pay dividends in other programming paradigms.

--I was happy with how this project turned out. The runtime for the program is satisfactory. Running it on a completely empty list, the first solution is found in under 3 seconds on my computer. It would be easy to tweak the program to output a (probably somewhat less than) infinite number of solutions.

-- *** Data Structures *** --

{-
  The data structures deserve some exposition. ring, clockwiseArc, and counterclockwiseArc (the reference lists, or reflists) are lists corresponding to the index numbers on the solution board (solboard) that each type corresponds to on the lotus. E.g. the second element of the clockwiseArc [0,7,15,22,30,37,45] can be located with solboard !! 7. The reflists are used for final solution validation, for the most part. There are 7 reflists in each, for a total of 21.

  The solution board (or solboard) appears frequently. It is represented by a list of Ints, where the indexes of the list refer to a positions on the lotus, and the value refers to the value at that position. Unmarked places on the solboard are represented by zeros through the duration of the program, as in the initial input.

  The reftree is a secondary data structure created to improve program runtime. The reftree has 49 nodes. Each node of the reftree contains three lists, one each from ring, clockwiseArc, and counterclockwiseArc, such that the index of that node is identical to the only number the three lists have in common. This is useful for generating valid positions; instead of processing the entire reflist every time to determine whether a candidate placement is valid, we can make sure of this index to restrain the comparisons to the three lists that are relevant.
-}

ring =  [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc =  [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc =  [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]

reflists = generateReflists ring clockwiseArc counterclockwiseArc
reftree = generateReftree ring clockwiseArc counterclockwiseArc [0..48]

-- *** Main Program Body *** --

main = do
  let testSolsValid = [[5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0],[4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0],[0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0],[4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0],[0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0],[0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]]
  let testSolInvalid = [0,0,3,7,6,1,4,4,0,5,1,7,0,0,0,2,6,0,4,5,3,1,0,0,2,0,0,5,6,0,7,0,0,2,3,2,3,0,7,6,0,0,0,4,5,6,1,7,0]

  --putStrLn $ show $ lotusSolver $ replicate 49 0
  putStrLn $ show $ map (\xs -> show (lotusSolver xs)) testSolsValid

{-
  First we check whether the solboard has anything in it; some of the supporting functions return an empty list. Then we check to see whether the solboard has any unfilled positions (does it have any zeros?). If it doesn't, we have a candidate solution and call validateSol.

  The main algorithm is recursive, unsuprisingly; a thread is started for each of the seven possible values. After each call of getSol there is one more value added to the board. Although every starting position is considered, it is not exhaustive; each candidate position is checked against the existing values on the board. If there is a conflict (i.e. that value has already been placed in one of the rings or arcs that index belongs to), that branch of the tree is not pursued any further (an empty list is returned for that branch and is subsumed by the other results).

  The takeOne helper function winnows down the results for solboards that have more than one solution. Thanks to the laziness of Haskell, once a result is found, the other threads cease evaluation.

  Input: [Int] representing the unsolved solution board.
  Output: [Int] with all solution values filled in (or [] if there is no solution found). Some puzzles have multiple solutions, but in every case only the first solution found is returned.
-}

lotusSolver::[Int] -> [Int]
lotusSolver solboard
  | length solboard == 0 = []
  | (checkForZeros solboard) = validateSol solboard
  | otherwise = (getSol 1 `takeOne` getSol 2 `takeOne` getSol 3 `takeOne` getSol 4 `takeOne` getSol 5 `takeOne` getSol 6 `takeOne` getSol 7)
    where getSol val = lotusSolver(getPlacement solboard val)
          checkForZeros solboard = length (filter (==0) solboard) == 0
          takeOne left right = if length left > 0 then left else right

{-
  Input: The solboard and the proposed value to place.
  Output: Returns the solboard. First we find the index of the first zero-valued item in the solboard. This is always how the next value is placed. If we can't find one, it's full; return it unmodified - this is a candidate solution. Then check that the placement is valid (meshes with the existing numbers on the board; return empty list if it is invalid. Otherwise, insert the new value at the position of the first zero found in the list and return the modified solboard.
-}
getPlacement::[Int]->Int->[Int]
getPlacement solboard val
  | (findIndex (==0) solboard == Nothing) = solboard
  | (length (validatePlacement solboard val (fromJust ((findIndex (==0) solboard)))) == 0) = []
  | otherwise =
    let (x,_:ys) = splitAt (fromJust ((findIndex (==0) solboard))) solboard
    in x ++ val:ys

{-
  Input: Takes the current solboard, the proposed value, and the index to place it at.
  Output: Here is where the reftree comes into play. We check each ring, LArc and RArc that the targeted index belongs to. maptrl gets the list of values from the current solboard corresponding to the indexes in the reflists. If the proposed value is an element of the solboard in any one of these lists of looked up values, return an empty list (failure condition). Duplicate values in any ring or arc violate the game rules. Otherwise return the solboard, unmodified (success condition).
-}
validatePlacement::[Int] -> Int -> Int -> [Int]
validatePlacement solboard val index
  | ((elem val (maprtl (refTreeLeaf 0) solboard)) || (elem val (maprtl (refTreeLeaf 1) solboard)) || (elem val (maprtl (refTreeLeaf 2) solboard))) = []
  | otherwise = solboard
    where refTreeLeaf i = (reftree !! index) !! i
          maprtl refTreeLeaf solboard = map (\index -> solboard !! index) refTreeLeaf

{-
  Input: Each of the three reflist groups, and a counter list [0..48].
  Output: Using the filterRefs helper function and a bit of recursion, take the reflists corresponding to each solboard index (as represented by the counter function) and stuff the three of them into the corresponding reftree node. Again, the idea is, reftree !! 7 gets you the three (and only three) reflists that contain 7.
-}
generateReftree::[[Int]] -> [[Int]] -> [[Int]] -> [Int] -> [[[Int]]]
generateReftree _ _ _ [] = []
generateReftree ring cArc ccArc (index:xs) = ((filterRefs index ring) ++ (filterRefs index cArc) ++ (filterRefs index ccArc)):(generateReftree ring cArc ccArc xs)
  where filterRefs i reflists = filter (\ys -> elem i ys) reflists

{-
  Input: The three reflist groups.
  Output: Returns a flattened list of the reflists. Part of the method here was to evenly distribute the rings and arcs so that when values were generated the values would not fill up the rings first before even touching the arcs. The way the program design went I don't think this was too important in the end.
-}
generateReflists::[[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
generateReflists [] [] [] = []
generateReflists (ring:xs) (cArc:ys) (ccArc:zs) = ring:cArc:ccArc:(generateReflists xs ys zs)

-- *** Solution validation *** --

{-
  The solution validator was the first thing I wrote, and it didn't change much in the course of development. There is a little overlap with the placement generation/validation functions, but different enough I didn't feel it made sense to try to unify them.
  Input: Candidate solboard.
  Output: If validation is successful, return unmodified solboard. If not, return empty list.
-}
validateSol::[Int] -> [Int]
validateSol solution
  | (validateLotus solution reflists) = solution
  | otherwise = []

{-
  This is the heart of the solution validator. The idea is to go through all of the reflists, generate a solution group for each (look up the values on the board for the indexes in the reflist in question), and validate each group.
  Input: The candidate solboard, and a list of all the reflists.
  Output: A verdict on the validity of the solution: here it is delivered as a recursive Bool consensus, True for valid, and False for invalid. If any group doesn't validate the result is False.
-}
validateLotus::[Int] -> [[Int]] -> Bool
validateLotus solution [] = True
validateLotus solution (reflist:xs) = validateSolutionGroup (generateSolutionGroup solution reflist) && (validateLotus solution xs)

{-
  Helper function for validateLotus. I think I probably would have coded it as a where clause had I written it later in development.
  Input: The candidate solgroup.
  Output: The solgroup, sorted, should be an identical match with [1..7]. This catches several different types of errors, such as numbers outside of the valid range and duplicate numbers. Another input validation: are there 7 and only 7 of each number in the solution? I think this actually follows if every solgroup checks out, but I won't attempt the proof.
-}
validateSolutionGroup::[Int] -> Bool
validateSolutionGroup solgroup
  | ((sort solgroup) == [1..7]) = True
  | otherwise = False

{-
  Helper function for validateLotus.
  Input: Takes a solboard, which is assumed to be complete (no zeros), and .
  Output: Generates a list of the corresponding values in the given solution.
-}
generateSolutionGroup::[Int]->[Int]->[Int]
generateSolutionGroup solution [] = []
generateSolutionGroup solution (index:xs) = (solution !! index):(generateSolutionGroup solution xs)
