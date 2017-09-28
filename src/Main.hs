{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

-- > heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack.git
-- > heroku config:set CHANNEL_SECRET={Channel Secret}
-- > heroku config:set CHANNEL_TOKEN={Access Token}

module Main where

import System.Environment

import Control.Monad
import Control.Concurrent.MVar

import Data.Maybe
import qualified Data.Text as T

import Network.Wai
import Network.Wai.Handler.Warp (run)

import Line.Messaging.API
import Line.Messaging.Webhook ( Event(..), EventMessage(..), ReplyToken(..)
                              , ReplyableEvent(..), webhookApp
                              , defaultOnFailure, getMessage, getReplyToken)
import Line.Messaging.Webhook.Types
import Line.Messaging.Types (Text(..), getText)
import Line.Messaging.Common.Types (ID)

type UserID = ID
type MV = MVar (Maybe (UserID, T.Text))

getChannelSecret :: IO ChannelSecret
getChannelSecret = T.pack <$> getEnv "CHANNEL_SECRET"

getChannelToken :: IO ChannelAccessToken
getChannelToken = T.pack <$> getEnv "CHANNEL_TOKEN"

main :: IO ()
main = do
  port <- maybe 80 read <$> lookupEnv "PORT" :: IO Int
  mv <- newMVar Nothing
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
      echo replyToken word -- [debug]
      return ()
      let userID = getID $ getSource event
      mbU'R <- takeMVar mv
      when (isJust mbU'R) $ do
        let (userID', word') = fromJust mbU'R
        when (userID' /= userID) $ echo replyToken "デ" -- word'
      putMVar mv $ Just (userID, word)
    _ -> echo replyToken "システムより：すみません、それには対応していません"

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

echo :: ReplyToken -> T.Text -> IO ()
echo replyToken content = do
  api $ reply replyToken [ Message . Text $ content ]
  return ()
