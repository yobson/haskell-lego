{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Robotics.Spike
import Debug.Trace

move :: SF Int (Int,Char)
move = proc dist -> do
  let speed = dist * 5
  let char  = head $ show $ speed `div` 13
  returnA -< (speed,char)

moveBack :: SF Int (Int,Char)
moveBack = constant (-30, 'B')

tooClose = proc dist -> 
  edge -< dist < 5

safe = proc dist -> 
  edge -< dist > 10

-- mainA = move1 ()
--   where move1 _ = (move     &&& tooClose) `switch` move2
--         move2 _ = (moveBack &&& safe)     `switch` move1

mainA = proc dist -> do
  t <- tooClose -< dist
  s <- safe     -< dist
  o <- rSwitch move -< (dist, lMerge (t `tag` moveBack)
                                     (s `tag` move))
  returnA -< o

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
    reactimate firstSample nextSample output mainA
  either print return out
