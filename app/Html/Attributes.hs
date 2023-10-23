{-# LANGUAGE OverloadedStrings #-}
module Html.Attributes
    ( attribute
    , className
    , Html.Attributes.id
    , listToText
    ) where

import Data.Text.Lazy (Text, intercalate, unwords)
import Html.Types

keyOf :: Attribute -> AttributeKey
keyOf (key, _) = key

valueOf :: Attribute -> AttributeValue
valueOf (_, val) = val

attribute :: AttributeKey -> AttributeValue -> Attribute
attribute key value = (key, value)

toText :: Attribute -> Text
toText attr =
    let 
        k = keyOf attr
        v = valueOf attr
    in intercalate "" [k, "=\"", v, "\""]

listToText :: [Attribute] -> Text
listToText = Data.Text.Lazy.unwords . map toText

-- Shorthands
className :: Text -> Attribute
className = attribute "class"

id :: Text -> Attribute
id = attribute "id"

