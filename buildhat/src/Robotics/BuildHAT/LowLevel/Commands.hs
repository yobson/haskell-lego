{-# LANGUAGE OverloadedStrings #-}

module Robotics.BuildHAT.LowLevel.Commands
( uploadFirmware
) where


import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B
import qualified System.Hardware.Serialport as S
import System.Hardware.Serialport hiding (send, recv)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Word
import Data.Bits
import Control.Monad
import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder

import Paths_buildhat

send s t = liftIO $ S.send s t

recv s l = liftIO $ S.recv s l

initialise :: (MonadIO m, MonadFail m) => SerialPort -> m ()
initialise s = do
  uploadFirmware s
  send s "echo 0\r"
  recv s 1024
  return ()


uploadFirmware :: (MonadIO m, MonadFail m) => SerialPort -> m ()
uploadFirmware s = do
  -- clear unwanted buffer
  send s "\r"
  liftIO $ threadDelay 250000
  line <- recv s 2048
  send s "version\r"
  liftIO $ threadDelay 250000
  line <- recv s 1024
  when ("version\r\nBuildHAT bootloader" `B.isPrefixOf` line) $ do
    firmware_path  <- liftIO $ getDataFileName "firmware.bin"
    signature_path <- liftIO $ getDataFileName "signature.bin"
    firmware <-  liftIO $ BS.readFile firmware_path
    signature <- liftIO $ BS.readFile signature_path
    send s $ loadCom (BS.length firmware) (checksum firmware)
    liftIO $ threadDelay 100000
    send s "\STX"
    send s firmware
    send s "\ETX\r"
    recv s $ BS.length firmware + 1024

    send s $ loadSig (BS.length signature)
    liftIO $ threadDelay 100000
    send s "\STX"
    send s signature
    send s "\ETX\r"
    liftIO $ threadDelay 100000
    recv s $ BS.length signature + 1024
    send s "reboot\r"
    recv s 4096
    return ()



checksum :: BS.ByteString -> Word64
checksum = BS.foldl inner 1
  where inner u b = (update u `xor` b') .&. 0xFFFFFFFF
                where b' = fromIntegral b
        update u | u .&. 0x80000000 /= 0 = (u `shift` 1) `xor` 0x1d872b41
                 | otherwise             =  u `shift` 1


loadCom :: Int -> Word64 -> BS.ByteString
loadCom len chk = B.toStrict $ toLazyByteString builder
  where builder = string8 "load " <> intDec len <> char8 ' ' <> word64Dec chk <> char8 '\r'

loadSig :: Int -> BS.ByteString
loadSig len = BS.toStrict $ toLazyByteString builder
  where builder = string8 "signature " <> intDec len <> char8 '\r'
