import Data.List

main = putStrLn $ validate solution
--main = putStrLn $ show (generateSolutionGroup solution (clockwiseArc !! 0))

ring = [[0..6],[7..13],[14..20],[21..27],[28..34],[35..41],[42..48]]
clockwiseArc = [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]
counterclockwiseArc = [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]
refboards = [ring,clockwiseArc,counterclockwiseArc]
refgroups = ring ++ clockwiseArc ++ counterclockwiseArc

solution = [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

--Takes in a reference group and generates a list of the corresponding values in the given solution
generateSolutionGroup::[Int]->[Int]->[Int]
generateSolutionGroup solution [] = []
generateSolutionGroup solution (index:xs) = (solution !! index):(generateSolutionGroup solution xs)

validateSolutionGroup::[Int] -> Bool
validateSolutionGroup solgroup
  | ((sort solgroup) == [1..7]) = True
  | otherwise = False

--Go through refgroups, generate a solution group for each one, and validate each group
validateLotus::[Int] -> [[Int]] -> Bool
validateLotus solution [] = True
validateLotus solution (refgroup:xs) = validateSolutionGroup (generateSolutionGroup solution refgroup) && (validateLotus solution xs)

--Other input validations: are the numbers between 1 and 7; are there 7 and only 7 of each number

validate::[Int] -> String
validate input
  | (length input /= 49) = "Invalid length"
  | otherwise = (if (validateLotus input refgroups) == True then "Valid solution" else "Invalid solution")
