lastButOne :: [a] -> a
lastButOne [a, b] = a
lastButOne (a : xs) = lastButOne xs