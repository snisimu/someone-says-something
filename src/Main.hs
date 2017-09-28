{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

-- > heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack.git
-- > heroku config:set CHANNEL_SECRET={Channel Secret}
-- > heroku config:set CHANNEL_TOKEN={Access Token}

module Main where

import System.Environment

import Control.Monad
import Control.Arrow
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
type Others'Mines = ([T.Text], [T.Text])
type MV = MVar (Map.Map (Maybe UserID) Others'Mines)

getChannelSecret :: IO ChannelSecret
getChannelSecret = T.pack <$> getEnv "CHANNEL_SECRET"

getChannelToken :: IO ChannelAccessToken
getChannelToken = T.pack <$> getEnv "CHANNEL_TOKEN"

main :: IO ()
main = do
  port <- maybe 80 read <$> lookupEnv "PORT" :: IO Int
  mv <- newMVar $ Map.singleton Nothing ([],[])
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
      let addTheWord = (:) word *** id
      eachUsers <- Map.map addTheWord <$> takeMVar mv
      let
        userID = getID $ getSource event
        forNewUser = fromJust $ Map.lookup Nothing eachUsers
        others'mines = tail' $ fromMaybe forNewUser $ Map.lookup (Just userID) eachUsers
      -- echo replyToken $ head' $ others'mines
      echo replyToken "debug"
      let
        appendTheWord = id *** flip (++) [word]
        eachUsers' = Map.insert (Just userID) (appendTheWord $ tail' others'mines) eachUsers
      putMVar mv eachUsers'
    _ -> echo replyToken "システムより：すみません、それには対応していません"
  where
  head' :: Others'Mines -> T.Text
  head' ([], mines) = head mines
  head' (others, _) = head others
  tail' :: Others'Mines -> Others'Mines
  tail' ([]      , mine:[]) = ([], [mine])
  tail' ([]      , mines  ) = ([], tail mines)
  tail' (other:[], mines  ) = ([other], mines)
  tail' (others  , mines  ) = (tail others, mines)

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

echo :: ReplyToken -> T.Text -> IO ()
echo replyToken word = do
  api $ reply replyToken [ Message $ Text word ]
  return ()
