import Control.Concurrent

main = do
  m <- newEmptyMVar
  forkIO $ do
    testCon m [1..20]
  putStrLn $ takeMVar m

testCon::IO (MVar [Int])->IO [Int]
testCon m [] = putMVar [1,2,3]
testCon m (x:_) = putMVar [4,5,6]
