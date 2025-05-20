import AATree

{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}
--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  -- split the data into words and build an AA tree
  -- use foldl
  let wordsList = words contents
      aatree = foldl (\t x -> insert x t) emptyTree wordsList

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  putStrLn $ "Size: " ++ show (size aatree)
  putStrLn $ "Height: " ++ show (height aatree)
  let n = size aatree
      optimalHeight = ceiling (logBase 2 (fromIntegral n + 1))
  putStrLn $ "Optimal height: " ++ show optimalHeight
  putStrLn $ "Height / Optimal height: " ++ show (fromIntegral (height aatree) / fromIntegral optimalHeight)
  putStrLn $ "checkTree: " ++ show (checkTree aatree)
  putStrLn $ "First 20 words: " ++ show (take 20 $ inorder aatree)

--------------------------------------------------------------------------------
