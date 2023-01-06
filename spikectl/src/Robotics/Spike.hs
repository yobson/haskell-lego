{-# LANGUAGE GeneralisedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Robotics.Spike 
( SpikeError
, SpikeT(..)
, Spike
, runSpike
, withSerial
, motorSetPwm
, motorSetSpeed
, motorSetAngle
, motorBrake
, eyesSetLeds
, eyesEnableLeds
, eyesGetDistance
, screenDraw
, screenPutChar
, screenPutStr
, screenPutStrDelay
, hubGetGyro

, Port(..)
) where


import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.IO.Class
import qualified System.Hardware.Serialport as P
import Control.Exception
import Data.Word
import Control.Concurrent

import qualified Robotics.Spike.LowLevel.IO as R
import Robotics.Spike.LowLevel.Command

type SpikeError = String

newtype SpikeT m a = SpikeT { unSpike :: ReaderT P.SerialPort (ExceptT SpikeError m) a }
  deriving (Functor,Applicative,Monad,MonadError SpikeError,MonadReader P.SerialPort)
type Spike a = SpikeT IO a

instance (Monad m) => MonadFail (SpikeT m) where
  fail = throwError

instance (MonadIO m) => MonadIO (SpikeT m) where
  liftIO xm = SpikeT $ ReaderT $ \r -> ExceptT $ do
    res <- liftIO $ try xm
    case res of
      Left e  -> return $ Left $ displayException (e :: IOException)
      Right x -> return $ Right x

instance MonadTrans SpikeT where
  lift = SpikeT . lift . lift

instance (MonadState s m) => MonadState s (SpikeT m) where
  get = lift get
  put = lift . put
  state = lift . state

runSpike :: (MonadIO m) => SpikeT m a -> P.SerialPort -> m (Either SpikeError a)
runSpike (SpikeT s) port = do
  liftIO $ P.recv port 2048
  liftIO $ P.send port "\3"
  liftIO $ threadDelay 1500000
  liftIO $ P.recv port 2048
  runExceptT $ runReaderT s port

withSerial :: (MonadIO m) => FilePath -> SpikeT m a -> m (Either SpikeError a)
withSerial f xm = do
  s <- liftIO $ P.openSerial f P.defaultSerialSettings{ P.commSpeed = P.CS115200 } 
  res <- runSpike xm s
  liftIO $ P.closeSerial s
  return res

motorSetPwm :: (MonadIO m) => Port -> Int -> SpikeT m ()
motorSetPwm port speed = ask >>= \s -> R.motorSetPwm s port speed

motorSetSpeed :: (MonadIO m) => Port -> Int -> SpikeT m ()
motorSetSpeed port speed = ask >>= \s -> R.motorRunAtSpeed s port speed

motorSetAngle :: (MonadIO m) => Port -> Int -> Int -> SpikeT m ()
motorSetAngle port angle speed = ask >>= \s -> R.motorRunToPos s port angle speed

motorBrake :: (MonadIO m) => Port -> SpikeT m ()
motorBrake port = ask >>= \s -> R.motorBrake s port

eyesSetLeds :: (MonadIO m) => Port -> (Word8,Word8,Word8,Word8) -> SpikeT m ()
eyesSetLeds port states = ask >>= \s -> R.distSetLED s port states

eyesEnableLeds :: (MonadIO m) => Port -> Bool -> SpikeT m ()
eyesEnableLeds port state = ask >>= \s -> R.distLEDs s port state

eyesGetDistance :: (MonadIO m) => Port -> SpikeT m Int
eyesGetDistance port = ask >>= \s -> R.distGet s port

screenDraw :: (MonadIO m) => [[Word8]] -> SpikeT m ()
screenDraw pic = ask >>= \s -> R.draw s pic

screenClear :: (MonadIO m) => SpikeT m ()
screenClear = ask >>= \s -> R.clearScreen s

screenPutChar :: (MonadIO m) => Char -> SpikeT m ()
screenPutChar c = ask >>= \s -> R.printR s c

screenPutStrDelay :: (MonadIO m) => Int -> String -> SpikeT m ()
screenPutStrDelay delay [] = return ()
screenPutStrDelay delay (x:xs) = do
  screenPutChar x
  mapM_ go xs
  where go c = do
          liftIO $ threadDelay (delay * 1000)
          screenPutChar c

screenPutStr :: (MonadIO m) => String -> SpikeT m ()
screenPutStr = screenPutStrDelay 400


hubGetGyro :: (MonadIO m) => SpikeT m (Int,Int,Int)
hubGetGyro = ask >>= \s -> R.getPosition s
