{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Discord
import Discord.Types
import Control.Monad (when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

import Bot.Types ( GlobalState(..), BotAction(..) )
import Bot.PingPong ( pingAction, pongAction )
import Bot.Dice (diceAction)
import Bot.MathExpr (calcAction)

handleEvent :: [BotAction GlobalState] -> GlobalState -> Event -> DiscordHandler ()
handleEvent actions state ev@(MessageCreate msg) = do
  let msgText = messageContent msg
  unless (userIsBot (messageAuthor msg)) $
    forM_ actions $ \BotAction{..} ->
      when (matchMsg state msgText) $
        runAction ev state
handleEvent _ _ _ = return ()

main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "DISCORD_TOKEN"
  let globalState = GlobalState
      actions = [pingAction, pongAction, diceAction, calcAction]
  err <- runDiscord $ def
    { discordToken = T.pack token
    , discordOnStart = liftIO $ putStrLn "Bot started"
    , discordOnEvent = handleEvent actions globalState
    }
  TIO.putStrLn err
