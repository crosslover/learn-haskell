import System.Environment (getArgs)

-- file: ch01/WC.hs
-- lines beginning with "--" are comments.
-- runghc WC -c < quux.txt
main =
  do
    (arg : _) <- getArgs
    let action = case arg of
          "-l" -> lineCount
          "-w" -> wordCount
          "-c" -> charCount
          "-fw" -> firstWord
    interact action

lineCount input = show (length (lines input)) ++ "\n"

wordCount input = show (length (words input)) ++ "\n"

charCount input = show (length input) ++ "\n"

firstWord = unlines . map (head . words) . lines