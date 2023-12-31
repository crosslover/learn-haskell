-- file: ch13/passwd-al.hs

import Control.Monad (when)
import Data.List
import System.Environment (getArgs)
import System.Exit
import System.IO

main = do
  -- Load the command-line arguments
  args <- getArgs 

  -- If we don't have the right amount of args, give an error and abort
  when (length args /= 2) $ do
    putStrLn "Syntax: passwd-al filename uid"
    exitFailure

  -- Read the file lazily
  content <- readFile (args !! 0)

  -- Compute the username in pure code
  let username = findByUID content (read (args !! 1))

  -- Display the result
  case username of
    Just x -> putStrLn x
    Nothing -> putStrLn "Could not find that UID"

-- Given the entire input and a UID, see if we can find a username.
findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let al = map parseline . lines $ content
   in lookup uid al

-- Convert a colon-separated line into fields
parseline :: String -> (Integer, String)
parseline input =
  let fields = split ':' input
   in (read (fields !! 2), fields !! 0)

-- Takes a delimiter and a list.
-- Break up the list based on the delimiter.
split :: Eq a => a -> [a] -> [[a]]
-- If the input is empty, the result is a list of empty lists.
split _ [] = [[]]
split delimiter str =
  let -- Find the part of the list before delimiter and put it in "before".
      -- The result of the list, including the leading delimiter, goes in "remainder".
      (before, remainder) = span (/= delimiter) str
   in before : case remainder of
        [] -> []
        x ->
          -- If there is more data to process,
          -- call split recursively to process it
          split delimiter (tail x)