module Main where

import Control.Concurrent (threadDelay)
import Hasql
import Hasql.TH
import Relude

settngs :: Settings
settngs = undefined

main :: IO ()
main =
  forever $ do
    result <- acquire settngs
    case result of
      Left err -> print err
      Right conn -> do
        let session =
              statement () [singletonStatement|select count(*) from users|]
        res <- run session conn
        case res of
          Left err -> print err
          Right cnt -> print cnt
        release conn
        threadDelay (60 * 1000000)
