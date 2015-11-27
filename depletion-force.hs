import Control.Monad
import Control.Monad.Random


boxWidth :: Float
boxWidth = 0.2

randomPosition :: (MonadRandom m) => m Float
randomPosition = getRandomR (0, 1)


-- try and sample a configuration with n boxes
sample :: (MonadRandom m) => Int -> m (Maybe Configuration)
sample n = throwBoxes n emptyConfiguration

-- try and extend the given configuration by throwing in another n boxes
throwBoxes :: (MonadRandom m) => Int -> Configuration -> m (Maybe Configuration)
throwBoxes 0 con = return (Just con)
throwBoxes n con = do
  mNextCon <- throwOneBox con
  case mNextCon of
    Nothing -> return Nothing
    Just nextCon -> throwBoxes (n-1) nextCon

throwOneBox :: (MonadRandom m) => Configuration -> m (Maybe Configuration)
throwOneBox con = do
  pos <- randomPosition
  return $ addBox pos con


-- positions of some boxes without collisions
newtype Configuration = Configuration [Float]
  deriving (Show, Read)

emptyConfiguration :: Configuration
emptyConfiguration = Configuration []

addBox :: Float -> Configuration -> Maybe Configuration
addBox pos (Configuration list) =
  if any (collision pos) list
    then Nothing
    else Just $ Configuration (pos : list)

collision :: Float -> Float -> Bool
collision pos1 pos2 = abs (pos1 - pos2) < boxWidth


partition :: Float -> Configuration -> [Int]
partition = goOnPartitioning 0

goOnPartitioning :: Float -> Float -> Configuration -> [Int]
goOnPartitioning offset step con@(Configuration list)
  | offset > 1  = []
  | otherwise   = (count inStep list)
      : goOnPartitioning (offset + step) step con
  where inStep x = offset <= x && x < offset + step

count :: (a -> Bool) -> [a] -> Int
count p []   = 0
count p (x:xs) = (if p x then 1 else 0) + count p xs


-- take numSamples samples with numBoxes boxes each,
-- partition them with step width partitionStep
-- and count overall boxes in each partition step
statistic :: (MonadRandom m) => Int -> Int -> Float -> m [Int]
statistic numBoxes numSamples partitionStep = do
  infiniteMSamples <- repeatM (sample numBoxes)
  let infiniteSamples = filterJust infiniteMSamples
      samples = take numSamples infiniteSamples
      partitionedSamples = map (partition partitionStep) samples
      summedPartitionedSamples = foldl (zipWith (+)) (repeat 0) partitionedSamples
  return summedPartitionedSamples

filterJust :: [Maybe a] -> [a]
filterJust ((Just x):mxs) = x : filterJust mxs
filterJust (Nothing:mxs)   = filterJust mxs

repeatM :: Monad m => m a -> m [a]
repeatM a = do
  x <- a
  xs <- repeatM a
  return (x:xs)

main :: IO ()
main = do
  stat <- evalRandIO (statistic 3 10000 0.05)
  putStrLn "FEEL THE DEPLETION FORCE"
  sequence_ $ map (putStrLn . flip replicate '#' . (`div` 100)) stat
