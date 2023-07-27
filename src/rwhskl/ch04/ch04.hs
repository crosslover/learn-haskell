{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
import Data.Char (digitToInt, isDigit)

safeHead :: [a] -> Maybe a
safeHead = safeProc head

safeTail :: [a] -> Maybe [a]
safeTail = safeProc tail

safeLast :: [a] -> Maybe a
safeLast = safeProc last

safeInit :: [a] -> Maybe [a]
safeInit = safeProc init

safeProc f [] = Nothing
safeProc f xs = Just (f xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs =
  let (xs1, xs2) = break p xs
      left = [xs1 | not (null xs1)]
   in case xs2 of
        [] -> left
        _ ->
          let (xs3, xs4) = span p xs2
           in case xs4 of
                [] -> left
                _ -> left ++ splitWith p xs4

transposition :: String -> String
transposition = unlines . doTransposition . lines
  where
    doTransposition :: [String] -> [String]
    doTransposition = foldr apd []
    apd :: String -> [String] -> [String]
    apd (a : xs) [] = [a] : apd xs []
    apd [] xss = xss
    apd (a : xs) (s : rs) = (a : s) : apd xs rs

asInt_fold :: String -> Int
asInt_fold = foldr parse 0
  where
    parse c i = case c of
      '-' -> -i
      _ -> digitToInt c + i * 10

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either = foldr parse (Right 0)
  where
    parse c r = case (c, r) of
      (_, left@(Left _)) -> left
      ('-', Right i) -> Right (-i)
      (_, Right i)
        | isDigit c -> Right (digitToInt c + i * 10)
        | otherwise -> Left ("non-digit " ++ show c)

concat :: [[a]] -> [a]
concat = foldr (++) []

takeWhile_recursion :: (a -> Bool) -> [a] -> [a]
takeWhile_recursion f [] = []
takeWhile_recursion f (a : xs) =
  let take = f a
   in if take
        then a : takeWhile_recursion f xs
        else []

takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold f = foldl take []
  where
    take r c = if f c then r ++ [c] else r
