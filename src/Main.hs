{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Discord
import Discord.Types
import Discord.Requests (ChannelRequest(CreateMessage))
import Control.Monad (when, unless, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getEnv)
import System.Timeout (timeout)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Maybe (isNothing)

import Bot.Types (GlobalState(..), BotAction(..))
import Bot.PingPong (pingAction, pongAction)
import Bot.Dice (diceAction)
import Bot.MathExpr (calcAction)
import Bot.FactorialDetector (factorialAction)
import Bot.PolinomeAction (polinoeAction)

-- | Runs a DiscordHandler with a timeout.
--   If it times out, a fallback message is sent to the specified channel.
runWithTimeout :: ChannelId -> Int -> DiscordHandler () -> DiscordHandler ()
runWithTimeout chan micros handler = do
  env <- ask
  result <- liftIO $ timeout micros (runReaderT handler env)
  when (isNothing result) $
    void $ restCall $ CreateMessage chan "⏱️ That took too long."

-- | Handles incoming Discord events and applies a timeout per action
handleEvent :: [BotAction GlobalState] -> GlobalState -> Event -> DiscordHandler ()
handleEvent actions state ev@(MessageCreate msg) = do
  let msgText = messageContent msg
      chan = messageChannelId msg
  unless (userIsBot (messageAuthor msg)) $
    forM_ actions $ \BotAction{..} ->
      when (matchMsg state msgText) $
        runWithTimeout chan (5 * 10^(6 :: Int)) (runAction ev state)
handleEvent _ _ _ = return ()

-- | Entry point of the Discord bot
main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "DISCORD_TOKEN"
  let globalState = GlobalState
      actions =
        [ pingAction
        , pongAction
        , diceAction
        , calcAction
        , factorialAction
        , polinoeAction
        ]
  err <- runDiscord $ def
    { discordToken = T.pack token
    , discordOnStart = liftIO $ putStrLn "Bot started"
    , discordOnEvent = handleEvent actions globalState
    }
  TIO.putStrLn err
