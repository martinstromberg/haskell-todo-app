{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy
import qualified Data.List as DL (partition)
import Data.Maybe
import qualified Data.Text as TS (Text)
import qualified Data.Text.Read as TSR (decimal)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Html
import Network.Wai
import Network.Wai.Parse
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Templates
import TodoItem

type TodoList = [TodoItem]
type IOTodoList = IORef TodoList

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain")]

notFound :: IO Response
notFound =
    return $ responseLBS status404 textPlain "404 Not Found"

badRequest :: IO Response
badRequest =
    return $ responseLBS status400 textPlain "400 Bad Request"

respondWithHtml :: Status -> ByteString -> Response
respondWithHtml status =
    responseLBS status [("Content-Type", "text/html;charset=utf-8")]

getTodoIndex :: IOTodoList -> Request -> IO Response
getTodoIndex todoRef req = do
    todos <- liftIO $ readIORef todoRef
    return $ respondWithHtml status200 $ renderDocument $ todoIndexPage todos

isHtmx :: Request -> Bool
isHtmx req =
    case lookup "HX-Request" $ requestHeaders req of
        Just val -> val == "true"
        Nothing -> False

getNextId :: TodoList -> Int
getNextId = (+) 1 . Prelude.maximum . Prelude.map idOf

postTodos :: IOTodoList -> Request -> IO Response
postTodos todoRef req = do
    (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
    case lookup "description" params of
        Just "" -> badRequest
        Just desc -> do
            todoItems <- readIORef todoRef
            let ti = TodoItem
                    { todoId = getNextId todoItems
                    , description = TLE.decodeUtf8 $ fromStrict desc
                    , completed = False
                    }
            liftIO $ writeIORef todoRef (todoItems ++ [ti])
            return
                $ respondWithHtml status200
                $ renderNode
                $ todoItemInList ti
        Nothing -> badRequest

updateCompleted :: Int -> Bool -> TodoList -> TodoList
updateCompleted _ _ [] = [] -- Nothing to do with an empty list
updateCompleted targetId isDone (x:xs)
    | idOf x == targetId = x { completed = isDone } : xs 
    | otherwise = x : updateCompleted targetId isDone xs

findTodoItem :: Int -> TodoList -> Maybe TodoItem
findTodoItem _ [] = Nothing
findTodoItem targetId (x:xs)
    | idOf x == targetId = Just x
    | otherwise = findTodoItem targetId xs

putTodo :: Int -> IOTodoList -> Request -> IO Response
putTodo id todoRef req = do
    todoItems <- readIORef todoRef
    case findTodoItem id todoItems of
        Just todoItem -> do
            (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
            let isDone = (==) "on" $ fromMaybe "" $ lookup "completed" params
            liftIO $ writeIORef todoRef $ updateCompleted id isDone todoItems
            return
                $ respondWithHtml status200
                $ renderNode
                $ todoItemInList todoItem { completed = isDone }
        Nothing -> notFound

deleteTodo :: Int -> IOTodoList -> Request -> IO Response
deleteTodo id todoRef req = do
    todoItems <- readIORef todoRef
    case findTodoItem id todoItems of
        Just _ -> do
            let (_, updated) = DL.partition (\t -> id == idOf t) todoItems
            liftIO $ writeIORef todoRef updated
            return $ responseLBS status200 textPlain ""
        Nothing -> notFound
    

slugToId :: TS.Text -> Maybe Int
slugToId slug =
    case TSR.decimal slug of
        Right (val, _) -> Just val
        Left _ -> Nothing


app :: IOTodoList -> Application
app todoRef req respond = do
    response <- case pathInfo req of
            [] -> getTodoIndex todoRef req
            ["todos"] -> case requestMethod req of
                    "GET" -> getTodoIndex todoRef req
                    "POST" -> postTodos todoRef req
                    _ -> notFound
            ["todos", slug] ->
                case slugToId slug of
                    Just id ->
                        case requestMethod req of
                            "PUT" -> putTodo id todoRef req
                            "DELETE" -> deleteTodo id todoRef req
                            _ -> notFound
                    Nothing -> notFound
            _ -> notFound
    respond response

main :: IO ()
main = do
    todoRef <- newIORef [TodoItem
                            { todoId = 1
                            , description = "Finish the demo"
                            , completed = False
                            }
                        ]
    putStrLn "Starting Todo app on port 8080..."
    run 8080 (app todoRef)
