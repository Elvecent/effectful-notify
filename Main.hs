module Main where

import Effectful.DBus.Notify

main = do
  client <- connectSession
  let startNote = appNote { summary = "Starting"
                          , body = Just $ Text "Calculating fib(40)."
                          , actions = Actions [ack] }
  let endNote = appNote { summary = "Finished"
                        , body = Just . Text . show $ fib40
                        , actions = Actions [ok] }
  let btwNote = appNote { summary = "By the way"
                        , body = Just . Text $ "Have a nice day!"
                        , actions = Actions [youToo, noThanks] }
  res <- runEff $ runNotify client $ do
    notify btwNote
    notification <- notify startNote
    fib40 `seq` replace notification endNote
  case res of
    Left (cs, e) -> do
      putStrLn $ "error: " <> show e
      print cs
    Right (sh, _) -> do
      getLine
      uninstallActionsHandler client sh
  where
    appNote = blankNote { appName = "Fibonacci Demonstration" }
    ack = Action "ack" "acknowledge" $ putStrLn "The user has acknowledged."
    ok = Action "ok" "okay" $ putStrLn "The user says okay."
    youToo = Action "youtoo" "you too" $ putStrLn "The user says you too."
    noThanks = Action "nothx" "no thanks" $ putStrLn "The user says no thanks."
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
    fib40 = fib 40
