import Control.Concurrent

main = do
  m <- newEmptyMVar
  forkIO testCon m [1..20]
  putStrLn $ takeMVar m

testCon::[Int]->IO [Int]
testCon = do
  putMVar m [4,5,6]
  return m

  data SolveRequest = SolveRequest (MVar [Int])

  initSolver :: IO SolveRequest
  initSolver = do
    m <- newEmptyMVar
    let s = SolveRequest m
    forkIO (solver s)
    return s

  solver :: SolveRequest -> IO [Int]
  solver (SolveRequest m) = do
      cmd <- takeMVar m
      putStrLn "solve: "
      putMVar m [1,2,3]

  getSol :: SolveRequest -> IO [Int]
  getSol (SolveRequest m) = do
    s <- newEmptyMVar
    putMVar m (SolveRequest s)
    takeMVar s
    --putStrLn s

  main = do
    s <- initSolver
    getSol s
