{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Lucid.Base
import Lucid.Html5
import Web.Scotty

main :: IO ()
main = do
  c <- newTVarIO 0

  scotty 3000 $
    get "/" $ do
      i <- liftIO $ atomically $ incr c
      raw $ renderBS $ (p_ (toHtml $ "Visitors: " <> show i))

incr :: TVar Int -> STM Int
incr c = stateTVar c (\x -> (x + 1, x + 1))
