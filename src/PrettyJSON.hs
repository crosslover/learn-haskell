module PrettyJSON
  ( renderJValue,
  )
where

import Prettify
import SimpleJSON (JValue (..))
import Prelude hiding ((<>))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where
    field (name, val) =
      string name
        <> text ": "
        <> renderJValue val
