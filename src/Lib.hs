module Lib where

import Input
import Person
import Time

dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ 
    ("hello", hello)
  ]

run :: IO ()
run = do
    (command : args) <- promptWords "Please enter a command followed by some arguments: "
    case command of 
        "hello" -> do
            hello args
            run
        "quit" -> do
            reallyWantsToQuit <- yesOrNo "Are you sure you want to quit?"
            if reallyWantsToQuit 
                then 
                    putStrLn "Okay, goodbye!"
                else do
                    putStrLn "Okay, I'll return you to the main menu!"
                    run
        _ -> do 
            putStrLn "Sorry, I don't understand that command!"
            run


hello :: [String] -> IO ()
hello [who] = putStrLn $ "Hello, " ++ who ++ "!"
hello [who, fromWhere] = putStrLn $ "Hello, " ++ who ++ " from " ++ fromWhere ++ "!"
hello _ = putStrLn "Usage: `hello <name> <place>?`"