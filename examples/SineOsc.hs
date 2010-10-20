{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

sineOsc :: SR ()
sineOsc = [$rel| () ->
  local o
  o = sin time
|]

main :: IO ()
main = do
  -- simulate defaultExperiment{execEngine = Interpreter} sineOsc
  simulate defaultExperiment sineOsc
  return ()

