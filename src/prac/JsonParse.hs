{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit, isSpace, toUpper)
import Data.List (dropWhileEnd)
import Debug.Trace (trace)
import Prelude hiding (error)

data JType
  = JNull
  | JNum Int
  | JString String
  | JArray [JType]
  | JObject [(String, JType)]

instance Show JType where
  show :: JType -> String
  show JNull = "JNULL"
  show (JNum i) = "JNUM " ++ show i
  show (JString s) = "JSTRING " ++ s
  show (JArray a) = "JARRAY " ++ show a
  show (JObject a) = "JOBJECT " ++ show a

-- type ES a = Either String a
newtype JParser a = JParser {run :: String -> (Either String a, String)}

instance Functor JParser where
  fmap :: (a -> b) -> JParser a -> JParser b
  fmap f (JParser p) = JParser $ \a ->
    let r@(e, s) = p a
     in case e of
          Right x -> (Right (f x), s)
          Left x -> (Left x, s)

instance Applicative JParser where
  pure :: a -> JParser a
  pure a = JParser (Right a,)
  (<*>) :: JParser (a -> b) -> JParser a -> JParser b
  (<*>) f a = JParser $ \x ->
    let (e, s) = run f x
     in case e of
          Right x ->
            let (ea, sa) = run a s
             in case ea of
                  Right xa -> (Right $ x xa, sa)
                  Left x -> (Left x, s)
          Left x -> (Left x, s)

instance Monad JParser where
  (>>=) :: JParser a -> (a -> JParser b) -> JParser b
  (>>=) a f = JParser $ \x ->
    let (e, s) = run a x
     in case e of
          Right x -> run (f x) s
          Left x -> (Left x, s)

instance MonadFail JParser where
  fail :: String -> JParser a
  fail = error

readJson :: String -> (Either String JType, String)
readJson =
  run
    ( do
        c <- nextChar
        case c of
          '"' -> JParser readJString
          '[' -> JParser readJArray
          '{' -> JParser readJObject
          _ -> do
            str <- JParser readString
            case c of
              'n' -> JParser (toJNull str,)
              _ -> JParser (toJNum str,)
    )

readChar :: String -> (Either String Char, String)
readChar [] = (Left "end", [])
readChar (x : xs) = (Right x, xs)

readJString :: String -> (Either String JType, String)
readJString =
  run
    ( do
        expect '"' (JParser readChar)
        s <- JParser readString
        expect '"' (JParser readChar)
        return (JString s)
    )

readJArray :: String -> (Either String JType, String)
readJArray =
  run
    ( do
        expect '[' (JParser readChar)
        a <- doReadArray
        expect ']' (JParser readChar)
        return $ JArray a
    )
  where
    doReadArray = do
      trimPre
      c <- nextChar
      case c of
        ']' -> do
          return []
        ',' -> do
          JParser readChar
          trimPre
          j <- JParser readJson
          js <- doReadArray
          return (j : js)
        _ -> do
          j <- JParser readJson
          js <- doReadArray
          return (j : js)

readJObject :: String -> (Either String JType, String)
readJObject =
  run
    ( do
        expect '{' (JParser readChar)
        a <- doReadEntryArray
        expect '}' (JParser readChar)
        return $ JObject a
    )
  where
    doReadEntryArray = do
      trimPre
      c <- nextChar
      case c of
        '}' -> do
          return []
        ',' -> do
          JParser readChar
          trimPre
          j <- doReadEntry
          js <- doReadEntryArray
          return (j : js)
        _ -> do
          j <- doReadEntry
          js <- doReadEntryArray
          return (j : js)
    doReadEntry = do
      (JString s) <- JParser readJString
      trimPre
      expect ':' (JParser readChar)
      trimPre
      o <- JParser readJson
      return (s, o)

readString :: String -> (Either String String, String)
readString [] = (Right "", "")
readString s =
  run
    ( do
        c <- JParser readChar
        case c of
          '\\' -> do
            c <- JParser readChar
            s <- JParser readString
            return (c : s)
          _ ->
            if c == '}' || c == ':' || c == '"' || c == ']'
              then
                ( do
                    st <- get
                    put (c : st)
                    return ""
                )
              else do
                s <- JParser readString
                return (c : s)
    )
    s

toJNull :: String -> Either String JType
toJNull s =
  let us = map toUpper s
   in case us of
        "NULL" -> Right JNull
        _ -> Left $ "error input:" ++ s

toJNum :: String -> Either String JType
toJNum s = Right $ JNum (read s :: Int)

put :: String -> JParser ()
put s = JParser $ const (Right (), s)

get :: JParser String
get = JParser $ \x -> (Right x, x)

error :: String -> JParser a
error s = JParser $ const (Left s, s)

expect :: (Eq a, Show a) => a -> JParser a -> JParser a
expect a (JParser fa) = JParser $ \x ->
  let (r, s) = fa x
   in case r of
        (Left _) -> (r, s)
        (Right rr) -> if rr == a then (r, s) else (Left ("expect:" ++ show a), s)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

trimPre = do
  s <- get
  put (dropWhile isSpace s)

nextChar = do
  (c : _) <- get
  return c

traceWrap a = trace (show a) a
