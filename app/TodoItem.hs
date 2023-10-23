module TodoItem
    ( TodoItem (..)
    , completedOf
    , descriptionOf
    , idOf
    ) where

import Data.Text.Lazy (Text)

data TodoItem = TodoItem
                { todoId        :: Int
                , description   :: Text
                , completed     :: Bool
                }

idOf :: TodoItem -> Int
idOf (TodoItem { todoId = id }) = id

descriptionOf :: TodoItem -> Text
descriptionOf (TodoItem { description = desc }) = desc

completedOf :: TodoItem -> Bool
completedOf (TodoItem { completed = done }) = done

