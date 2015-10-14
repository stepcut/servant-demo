module Main where

import Person (app)
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8000 app
