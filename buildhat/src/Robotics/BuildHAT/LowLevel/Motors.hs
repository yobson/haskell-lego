

module Robotics.BuildHAT.LowLevel.Motors where

import Robotics.BuildHAT.LowLevel.Commands
import System.Hardware.Serialport   hiding (send, recv)
import Control.Monad
import Control.Monad.IO.Class

-- sendCom s = send s . command
sendCom s c = do
  let com = command c
  liftIO $ print com
  send s com

setSpeed :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Float -> m ()
setSpeed _ _ speed | speed > 100 || speed < -100 = fail "Speed out of range, [-100,100]"
setSpeed s p speed = void $ sendCom s $ Port p :> Pwm :> Set (Val (speed / 100))

stop :: (MonadIO m) => SerialPort -> Port -> m ()
stop s p = void $ sendCom s $ Port p :> Off

plimit :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Float -> m ()
plimit _ _ l | l < 0 || l > 1 = fail "plimit out of range [0,1]"
plimit s p l = void $ sendCom s $ Port p :> PLimit l

bias :: (MonadIO m, MonadFail m) => SerialPort -> Port -> Float -> m ()
bias _ _ l | l < 0 || l > 1 = fail "bias out of range [0,1]"
bias s p l = void $ sendCom s $ Port p :> Bias l

runForDegrees :: (MonadIO m, MonadFail m) 
              => SerialPort
              -> Port
              -> Float -- ^ Degrees
              -> Float -- ^ Speed
              -> m ()
runForDegrees _ _ _ sp | sp < -100 || sp > 100 = fail "Speed out of range [-100,100]"
runForDegrees s p d sp = undefined
