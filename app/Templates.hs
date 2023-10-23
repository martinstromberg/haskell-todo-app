{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Data.Text.Lazy
import Html
import Html.Attributes
import Html.Types
import TodoItem

todoItemInList :: TodoItem -> Node
todoItemInList item =
    let url = "/todos/" `append` pack (show (idOf item))
        attrs =
            let a = [ attribute "hx-put" url
                    , attribute "hx-swap" "outerHTML"
                    , attribute "hx-target" "closest .todo-item"
                    , attribute "name" "completed"
                    , attribute "type" "checkbox"
                    ]
                checked = attribute "checked" "true"
            in if completedOf item then checked : a else a
    in li
        [ className "todo-item" ]
        [ form
            []
            [ label
                []
                [ input attrs
                , text (descriptionOf item)
                ]
            , text "&nbsp;|&nbsp"
            , button 
                [ attribute "hx-delete" url
                , attribute "hx-swap" "delete"
                , attribute "hx-target" "closest .todo-item"
                ]
                [ text "Delete"]
            ]
        ]

todoItemList :: [TodoItem] -> Node
todoItemList items =
    let f = li
            []
            [ form
                [ attribute "hx-on::after-request" "this.reset()"
                , attribute "hx-post" "/todos"
                , attribute "hx-swap" "beforebegin"
                , attribute "hx-target" "closest li"
                ]
                [ input
                    [ attribute "name" "description"
                    , attribute "type" "text"
                    ]
                , input
                    [ attribute "type" "submit"
                    , attribute "value" "Save"
                    ]
                ]
            ]
        n = Prelude.map todoItemInList items ++ [f]
    in Html.div
        [ className "todo-items-index"]
        [ ul
            [className "todo-item-list"]
            n
        ]

todoIndexPage :: [TodoItem] -> Node
todoIndexPage items =
    html
        [ attribute "lang" "en" ]
        [ Html.head
            [ element "meta" [attribute "charset" "utf-8"] []
            , title "Todos - Haskell + Htmx"
            ]
        , body
            []
            [ element "header" [] [ element "h1" [] [ text "Haskell + HTMX" ] ]
            , element "main" [] [ todoItemList items ]
            , element
                "footer"
                []
                [ text "Powered by "
                , element
                    "a"
                    [ attribute "href" "https://www.haskell.org" ]
                    [ text "Haskell" ]
                ]
            , element
                "script"
                [ attribute "src" "https://unpkg.com/htmx.org@1.9.6"
                , attribute "integrity" "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni"
                , attribute "crossorigin" "anonymous"
                ]
                [text ""]
            ]
        ]

