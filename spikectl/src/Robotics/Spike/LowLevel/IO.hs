module Robotics.Spike.LowLevel.IO
( motorSetPwm
, motorRunAtSpeed
, motorRunToPos
, motorBrake
, distSetLED
, distLEDs
, distGet
, draw
, clearScreen
, printR
, getPosition
) where


import System.IO
import Data.Word
import qualified Data.ByteString as B
import Control.Monad.IO.Class
import System.Hardware.Serialport

import Robotics.Spike.LowLevel.Command


motorSetPwm :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Int -> m ()
motorSetPwm s port speed = do
  let com = buildCommand $ PortCom port $ MAction $ SetPWM speed
  liftIO $ send s com
  chkError com s

motorRunAtSpeed :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Int -> m ()
motorRunAtSpeed s port speed = do
  let com = buildCommand $ PortCom port $ MAction $ RunAtSpeed speed
  liftIO $ send s com
  chkError com s

motorRunToPos :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Int -> Int -> m ()
motorRunToPos s port pos speed = do
  let com = buildCommand $ PortCom port $ MAction $ RunToPos pos speed
  liftIO $ send s com
  chkError com s

motorBrake :: (MonadIO m, MonadFail m) => SerialPort -> Port -> m ()
motorBrake s port = do
  let com = buildCommand $ PortCom port $ MAction $ Brake
  liftIO $ send s com
  chkError com s

distSetLED :: (MonadIO m, MonadFail m) => SerialPort -> Port -> (Word8, Word8, Word8, Word8) -> m ()
distSetLED s port (tl, tr, bl, br) = do
  let com = buildCommand $ PortCom port $ DistAction $ DistLED tl tr bl br
  liftIO $ send s com
  chkError com s

distLEDs :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Bool -> m ()
distLEDs s port on = do
  let com = buildCommand $ PortCom port $ DistAction $ 
              if on then DistLEDsOn
                    else DistLEDsOff
  liftIO $ send s com
  chkError com s

distGet :: (MonadIO m, MonadFail m) => SerialPort -> Port -> m Int
distGet s port = do
  let com = buildCommand $ PortCom port $ DistAction DistGet
  liftIO $ send s com
  parseUnit com s

draw :: (MonadIO m, MonadFail m) => SerialPort -> [[Word8]] -> m ()
draw s pic = do
  let com = buildCommand $ Display $ Draw pic
  liftIO $ send s com
  chkError com s

clearScreen :: (MonadIO m, MonadFail m) => SerialPort -> m ()
clearScreen s = do
  let com = buildCommand $ Display ClearScreen
  liftIO $ send s com
  chkError com s

printR :: (MonadIO m, MonadFail m) => SerialPort -> Char -> m ()
printR s pic = do
  let com = buildCommand $ Display $ Print [pic]
  liftIO $ send s com
  chkError com s

getPosition :: (MonadIO m, MonadFail m) => SerialPort -> m (Int,Int,Int)
getPosition s = do
  let com = buildCommand $ Motion Position
  liftIO $ send s com
  parse3Tuple com s
