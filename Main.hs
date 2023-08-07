module Main where

import           Effectful
import           Effectful.DBus.Notify
import           Effectful.Error.Dynamic

import           Control.Concurrent      (threadDelay)

main :: IO ()
main = do
  client <- connectSession
  let startNote = appNote { summary = "Starting"
                          , body = Just $ Text "Calculating..."
                          , actions = Actions [ack]
                          , expiry = Milliseconds 3000 }
  let endNote = appNote { summary = "Finished"
                        , body = Just . Text $ "done!"
                        , actions = Actions [ok] }
  let btwNote = appNote { summary = "By the way"
                        , body = Just . Text $ "Have a nice day!"
                        , actions = Actions [youToo, noThanks] }
  res <- runEff $ runError @NotifyError $ runNotify client $ do
    _ <- notify btwNote
    notification <- notify startNote
    liftIO $ threadDelay 5000000
    replace notification endNote
  case res of
    Left (cs, e) -> do
      putStrLn $ "error: " <> show e
      print cs
    Right (sh, _) -> do
      _ <- getLine
      uninstallActionsHandler client sh
  where
    appNote = blankNote { appName = "Notifications Demonstration" }
    ack = Action "ack" "acknowledge" $ putStrLn "The user has acknowledged."
    ok = Action "ok" "okay" $ putStrLn "The user says okay."
    youToo = Action "youtoo" "you too" $ putStrLn "The user says you too."
    noThanks = Action "nothx" "no thanks" $ putStrLn "The user says no thanks."
