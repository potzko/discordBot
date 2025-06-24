{-# LANGUAGE OverloadedStrings #-}
module Bot.Util (sendMessageSafe) where

import Discord
import Discord.Types
import Discord.Requests (ChannelRequest(CreateMessage))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

-- | Send a message, but if it's over 2000 characters, print a warning and do not send.
sendMessageSafe :: String -> ChannelId -> T.Text -> DiscordHandler ()
sendMessageSafe actionName chan txt = do
  let len = T.length txt
  if len > 2000
    then do
      let preview =
            if len > 500
              then T.take 250 txt <> "..." <> T.drop (len - 250) txt
              else txt
      liftIO $ putStrLn $ "[DiscordBot] Action '" ++ actionName ++ "' tried to send a message of length " ++ show len ++ " (not sent to Discord):\n" ++ T.unpack preview
    else void $ restCall (CreateMessage chan txt) 
