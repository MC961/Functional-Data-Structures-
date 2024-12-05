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

-- Check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False

-- Time a single operation with detailed output
timeOp :: String -> IO a -> IO a
timeOp label action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diffNano :: Integer
        diffNano = end - start -- Difference in picoseconds
    let diffMicro :: Double
        diffMicro = fromIntegral diffNano / (10^6) -- Convert to microseconds
    let diffMilli :: Double
        diffMilli = fromIntegral diffNano / (10^9) -- Convert to milliseconds
    let diffSeconds :: Double
        diffSeconds = fromIntegral diffNano / (10^12) -- Convert to seconds
    printf "%s execution time: %.0f ns (%.3f µs, %.6f ms, %.9f s)\n"
        label (fromIntegral diffNano :: Double) diffMicro diffMilli diffSeconds
    return result

-- Testing the Functional Stack
testStack :: IO ()
testStack = do
    -- Create an empty stack
    let stack = emptyStack :: Stack Int -- Specify type as Stack Int
    putStrLn $ "Initial stack: " ++ show stack

    -- Time the push operations
    stack1 <- timeOp "Push 10" $ return (push 10 stack)
    stack2 <- timeOp "Push 20" $ return (push 20 stack1)
    stack3 <- timeOp "Push 30" $ return (push 30 stack2)
    putStrLn $ "After pushing 30: " ++ show stack3

    -- Time the pop operations
    (top1, stack4) <- timeOp "Pop" $ return (pop stack3)
    putStrLn $ "Popped item: " ++ show top1 ++ ", Remaining stack: " ++ show stack4

    (top2, stack5) <- timeOp "Pop again" $ return (pop stack4)
    putStrLn $ "Popped item: " ++ show top2 ++ ", Remaining stack: " ++ show stack5

    -- Time the peek operation
    top3 <- timeOp "Peek" $ return (peek stack5)
    putStrLn $ "Top item: " ++ show top3 ++ ", Stack remains: " ++ show stack5

    -- Check if the stack is empty
    putStrLn $ "Is stack empty? " ++ show (isEmpty stack)  -- True
    putStrLn $ "Is stack5 empty? " ++ show (isEmpty stack5) -- False

    -- Attempt to pop from an empty stack
    (top4, stack6) <- timeOp "Pop from empty stack" $ return (pop (emptyStack :: Stack Int))
    putStrLn $ "Popped item from empty stack: " ++ show top4 ++ ", Remaining stack: " ++ show stack6
