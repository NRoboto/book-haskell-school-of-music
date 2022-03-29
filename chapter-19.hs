{-# LANGUAGE Arrows #-}

import Euterpea

outputAsMono :: Mono AudRate -> IO ()
outputAsMono = outFile "test.wav" 5

sine440 :: Clock c => SigFun c () Double
sine440 = proc () -> do
  s <- oscFixed 440 -< ()
  outA -< s

outputSine440 = outputAsMono sine440

sine440WithOveratures :: Clock c => SigFun c () Double
sine440WithOveratures = proc () -> do
  s <- osc tab 0 -< 440
  outA -< s
  where
    tab = tableSinesN 4096 [1.0, 0.5, 0.33]

outputSine440WithOveratures = outputAsMono sine440WithOveratures