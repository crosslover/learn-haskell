module GlobRegex
  ( globToRegex,
    matchesGlob,
  )
where

import Control.Exception (SomeException, handle)
import Control.Monad (forM)
import Data.Char (toLower)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
  )
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*' : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?' : cs) = '.' : globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('[' : c : cs) = '[' : c : charClass cs
globToRegex' ('[' : _) = error "unterminated character class"
globToRegex' (c : cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c
  | c `elem` regexChars = '\\' : [c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c : cs) = c : charClass cs
charClass [] = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

isPattern :: String -> Bool
isPattern = any (`elem` "[?*")

namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return (if exists then [pat] else [])
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, baseName) -> do
          dirs <-
            if isPattern dirName
              then namesMatching (dropTrailingPathSeparator dirName)
              else return [dirName]
          let listDir =
                if isPattern baseName
                  then listMatches
                  else listPlain
          pathNames <- forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
          return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  handle (const (return []) :: (SomeException -> IO [String])) $
    do
      names <- getDirectoryContents dirName'
      let names' =
            if isHidden pat
              then filter isHidden names
              else filter (not . isHidden) names
      return (filter (`matchesGlob` pat) names')

isHidden ('.' : _) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])

-- prac
globToRegexCaseSensitive :: String -> Bool -> String
globToRegexCaseSensitive s caseSensitive = globToRegex (if caseSensitive then s else map toLower s)

matchesGlobCaseSensitive :: FilePath -> String -> Bool -> Bool
matchesGlobCaseSensitive name pat caseSensitive = (if caseSensitive then name else map toLower name) `matchesGlob` (if caseSensitive then pat else map toLower pat)