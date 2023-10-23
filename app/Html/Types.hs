module Html.Types
    ( Attribute
    , AttributeKey
    , AttributeValue
    , Element(..)
    , Node
    , TagName
    ) where

import Data.Either
import Data.Text.Lazy (Text)

-- Attributes
type AttributeKey = Text
type AttributeValue = Text
type Attribute = (AttributeKey, AttributeValue)

-- Nodes
type TagName = Text
data Element = Element
    { tagName       :: TagName
    , attributes    :: [Attribute]
    , children      :: [Node]
    }

type Node = Either Text Element

