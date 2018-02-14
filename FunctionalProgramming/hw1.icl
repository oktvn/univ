module hw1
import StdEnv 

// This is the solution I submitted
// It follows a pre-defined formula 

multisum :: Int -> Int
multisum n
| n == 0 = 1
| n > 0 = n * (n + 1) * (2*n + 1) / 6

Start = multisum 3
