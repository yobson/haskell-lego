{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Robotics.Spike
import Debug.Trace

mainSig :: SF Int (Int, Char)
mainSig = proc x -> do
  let speed = x * 5
  let char  = head $ show $ speed `div` 13
  returnA -< (min speed 127, char)


firstSample :: Spike Int
firstSample = return 0

nextSample :: Bool -> Spike (Double, Maybe Int)
nextSample _ = do
  dist <- eyesGetDistance PortB
  return (0.2, Just dist)


output :: Bool -> (Int, Char) -> Spike Bool
output _ (x,c) = do
  motorSetPwm PortA x
  screenPutChar c
  return False

main :: IO ()
main = do
  out <- withSerial "/dev/tty.usbmodem3784325C33381" $ 
    reactimate firstSample nextSample output mainSig
  either print return out
