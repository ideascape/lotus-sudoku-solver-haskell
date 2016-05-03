import System.Random
import Control.Monad.State

--main = putStrLn $ show (runState threeCoins (mkStdGen 33)) ++ show (runState threeCoins (mkStdGen 33))
main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('1','7') gen)


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
