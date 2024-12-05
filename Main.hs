import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.DeepSeq (NFData, deepseq, rnf)

-- Define a functional list
data FunctionalList a = Nil | Cons a (FunctionalList a)
    deriving (Show)

-- Convert from a normal list to a FunctionalList
fromList :: [a] -> FunctionalList a

fromList = foldr Cons Nil

-- Create a FunctionalList with n elements (1 to n)
createList :: Int -> FunctionalList Int
createList n = createList' n Nil
  where
    createList' 0 lst = lst
    createList' x lst = createList' (x - 1) (Cons x lst)

-- Traverse a FunctionalList (return all elements in a new list)
traverseList :: FunctionalList a -> [a]
traverseList Nil = []
traverseList (Cons x xs) = x : traverseList xs

-- Prepend an element to a FunctionalList
prepend :: Int -> FunctionalList Int -> FunctionalList Int
prepend x lst = Cons x lst

-- Append an element to the FunctionalList
append :: a -> FunctionalList a -> FunctionalList a
append x Nil = Cons x Nil
append x (Cons y ys) = Cons y (append x ys)

-- Map a function over a FunctionalList
fmapList :: (a -> b) -> FunctionalList a -> FunctionalList b
fmapList _ Nil = Nil
fmapList f (Cons x xs) = Cons (f x) (fmapList f xs)

-- Filter a FunctionalList
filterList :: (a -> Bool) -> FunctionalList a -> FunctionalList a
filterList _ Nil = Nil
filterList p (Cons x xs)
    | p x       = Cons x (filterList p xs)
    | otherwise = filterList p xs

-- Fold (reduce) a FunctionalList
foldList :: (b -> a -> b) -> b -> FunctionalList a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x xs) = foldList f (f acc x) xs

-- Reverse a FunctionalList
reverseList :: FunctionalList a -> FunctionalList a
reverseList = foldList (flip Cons) Nil

-- Force full evaluation to avoid lazy evaluation issues
instance NFData a => NFData (FunctionalList a) where
    rnf Nil = ()
    rnf (Cons x xs) = x `deepseq` rnf xs

-- Time measurement
measureTime :: IO () -> IO Double
measureTime action = do
    start <- getCPUTime
    action
    end <- getCPUTime
    return $ fromIntegral (end - start) / (10^12) -- Convert to seconds

-- Example usage with large sample
main :: IO ()
main = do
    let largeList = [1..1000000] -- Large input list with 1,000,000 elements
    let funcList = fromList largeList

    -- Measure performance for various operations (averaged over 5 runs)
    let numIterations = 5

    -- Time Create
    timeCreate <- measureTime $ do
        let createdList = createList 1000000
        createdList `deepseq` return () -- Force full evaluation

    -- Time Traverse
    timeTraverse <- measureTime $ do
        let traversedList = traverseList funcList
        traversedList `deepseq` return () -- Force full evaluation

    -- Time Prepend
    timePrepend <- measureTime $ do
        let prependedList = prepend 0 funcList
        prependedList `deepseq` return () -- Force full evaluation

    -- Time Append
    timeAppend <- measureTime $ do
        let appendedList = append 1000001 funcList
        appendedList `deepseq` return () -- Force full evaluation

    -- Time Map
    timeMap <- measureTime $ do
        let mappedList = fmapList (*2) funcList
        mappedList `deepseq` return () -- Force full evaluation

    -- Time Filter
    timeFilter <- measureTime $ do
        let filteredList = filterList even funcList
        filteredList `deepseq` return () -- Force full evaluation

    -- Time Reverse
    timeReverse <- measureTime $ do
        let reversedList = reverseList funcList
        reversedList `deepseq` return () -- Force full evaluation

    -- Output timing
    putStrLn $ printf "Time for create: %.6f seconds" timeCreate
    putStrLn $ printf "Time for traverse: %.6f seconds" timeTraverse
    putStrLn $ printf "Time for prepend: %.6f seconds" timePrepend
    putStrLn $ printf "Time for append: %.6f seconds" timeAppend
    putStrLn $ printf "Time for map: %.6f seconds" timeMap
    putStrLn $ printf "Time for filter: %.6f seconds" timeFilter
    putStrLn $ printf "Time for reverse: %.6f seconds" timeReverse
