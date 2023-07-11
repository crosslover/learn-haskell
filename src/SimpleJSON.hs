{-# LANGUAGE FlexibleInstances #-}

module SimpleJSON
  ( JValue (..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull,
  )
where

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber i) = Just (truncate i)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON String where
  toJValue = JString

  fromJValue (JString s) = Right s
  fromJValue _ = Left "not a JSON string"

result :: JValue
result =
  JObject
    [ ("query", JString "awkward squad haskell"),
      ("estimatedCount", JNumber 3920),
      ("moreResults", JBool True),
      ( "results",
        JArray
          [ JObject
              [ ("title", JString "Simon Peyton Jones: papers"),
                ("snippet", JString "Tackling the awkward ..."),
                ("url", JString "http://.../marktoberdorf/")
              ]
          ]
      )
    ]
