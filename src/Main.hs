{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

-- > heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack.git
-- > heroku config:set CHANNEL_SECRET={Channel Secret}
-- > heroku config:set CHANNEL_TOKEN={Access Token}

module Main where

import System.Environment

import Control.Monad
import Control.Concurrent.MVar

import Data.Maybe
import Data.Map.Strict as Map
import qualified Data.Text as T

import Network.Wai
import Network.Wai.Handler.Warp (run)

import Line.Messaging.API
import Line.Messaging.Webhook
import Line.Messaging.Common.Types (ID)

type UserID = ID
type MV = MVar ([T.Text], Map.Map UserID ([T.Text], [T.Text]))

getChannelSecret :: IO ChannelSecret
getChannelSecret = T.pack <$> getEnv "CHANNEL_SECRET"

getChannelToken :: IO ChannelAccessToken
getChannelToken = T.pack <$> getEnv "CHANNEL_TOKEN"

main :: IO ()
main = do
  port <- maybe 80 read <$> lookupEnv "PORT" :: IO Int
  mv <- newMVar ([], Map.empty)
  run port $ lineBot mv

lineBot :: MV -> Application
lineBot mv req f = do
  channelSecret <- getChannelSecret
  webhookApp channelSecret (handler mv) defaultOnFailure req f

handler :: MV -> [Event] -> IO ()
handler mv events = forM_ events (handleEvent mv)

handleEvent :: MV -> Event -> IO ()
handleEvent mv (MessageEvent event) = (handleMessageEvent mv) event
handleEvent _ _ = return ()

handleMessageEvent :: MV -> ReplyableEvent EventMessage -> IO ()
handleMessageEvent mv event = do
  let replyToken = getReplyToken event
  case getMessage event of
    TextEM mid (Text word) -> do
      -- echo replyToken word -- [debug]
      let userID = getID $ getSource event
      (allWords, eachUsers) <- takeMVar mv
      let add = Map.map $ \(words, myWords) -> (words ++ [word], myWords)
      mod <- case Map.lookup userID eachUsers of
        Just ([], []) -> do
          return $ Map.insert userID ([],[word])
        Just ([], word':words) -> do
          echo replyToken word'
          return $ Map.insert userID ([], words++[word])
        Just (word':words, myWords) -> do
          echo replyToken word'
          return $ Map.insert userID (words, myWords++[word])
        Nothing -> do
          case allWords of
            [] -> return $ Map.insert userID ([], [word])
            word':words -> do
              echo replyToken word'
              return $ Map.insert userID (words, [word])
      putMVar mv (allWords ++ [word], mod $ add eachUsers)
    _ -> echo replyToken "システムより：すみません、それには対応していません"

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

echo :: ReplyToken -> T.Text -> IO ()
echo replyToken word = do
  api $ reply replyToken [ Message $ Text word ]
  return ()
