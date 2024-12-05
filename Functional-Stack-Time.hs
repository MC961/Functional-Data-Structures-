import System.CPUTime
import Text.Printf

-- Define the Functional Stack as a list
data Stack a = Stack [a]
    deriving (Show)

-- Create an empty stack
emptyStack :: Stack a
emptyStack = Stack []

-- Push an element onto the stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- Pop an element from the stack
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack []) -- Handle empty stack
pop (Stack (x:xs)) = (Just x, Stack xs)

-- Peek the top element without modifying the stack
peek :: Stack a -> Maybe a
peek (Stack [])     = Nothing
peek (Stack (x:_))  = Just x

-- Time an operation
timeOp :: String -> IO a -> IO a
timeOp label action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diffNano = end - start
    let diffMicro = fromIntegral diffNano / (10^6) :: Double -- Convert to microseconds
    let diffMilli = fromIntegral diffNano / (10^9) :: Double -- Convert to milliseconds
    printf "%s execution time: %.0f ns (%.3f µs, %.6f ms)\n" label (fromIntegral diffNano :: Double) diffMicro diffMilli
    return result

-- Perform multiple operations to aggregate time
bulkOperations :: Stack Int -> Int -> Stack Int
bulkOperations stack 0 = stack
bulkOperations stack n = bulkOperations (push n stack) (n - 1)

testPerformance :: IO ()
testPerformance = do
    -- Measure multiple push operations
    let stack = emptyStack :: Stack Int
    timeOp "100,000 push operations" $ do
        let finalStack = bulkOperations stack 100000
        print finalStack
        return ()

    -- Measure multiple pop operations
    let stackFull = bulkOperations stack 100000
    timeOp "100,000 pop operations" $ do
        let finalStack = foldl (\s _ -> snd (pop s)) stackFull [1..100000]
        print finalStack
        return ()

main :: IO ()
main = testPerformance