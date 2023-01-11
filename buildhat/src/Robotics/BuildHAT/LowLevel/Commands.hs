{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Robotics.BuildHAT.LowLevel.Commands
( initialise
, uploadFirmware
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
import Control.Applicative

import Paths_buildhat

send s t = liftIO $ S.send s t

recv s l = liftIO $ S.recv s l

data Expr = Echo !Bool
          | Version
          | Port !Int
          | Vin
          | LedMode !Int
          | List
          | ClearFaults
          | Coast
          | Pwm
          | On
          | Off
          | Pid !PidParams
          | Set !SetPoint
          | Bias !Float
          | PLimit !Float
          | Select !(Maybe SelMode)
          | Selonce !(Maybe SelMode)
          | Combi !Int [(Int,Int)]
          | Write1 ![Word8]
          | Write2 ![Word8]
          | !Expr :> !Expr
          deriving (Show)
infixr 5 :>

data PidParams  = PidParams
  { pvport   :: !Int
  , pvmode   :: !Int
  , pvoffset :: !Int
  , pvformat :: !Format
  , pvscale  :: !Float
  , pvunwrap :: !Int
  , kp       :: !Float
  , ki       :: !Float
  , kd       :: !Float
  , windup   :: !Float
  } deriving (Show)

data Format = Format Bool -- ^ Signed?
                     BLen -- ^ Number of Bytes
  deriving Show

data BLen = B1 | B2 | B4
  deriving Show

data SetPoint = Val Float | WaveForm Wave
          deriving (Show)

data Wave = Square   Float Float Float Float
          | Sine     Float Float Float Float
          | Triangle Float Float Float Float
          | Pulse    Float Float Float
          | Ramp     Float Float Float
          deriving (Show)

data SelMode = Sel Int | SelOff Int Format
          deriving (Show)

render :: (MonadFail m) => Expr -> m Builder
render (Echo True)   = return $ string8 "echo 1"
render (Echo False)  = return $ string8 "echo 0"
render Version       = return $ string8 "version"
render (Port p)      | p < 0 || p > 3 = fail "Port out of range [0,3]"
                     | otherwise      = return $ string8 "port " <> intDec p
render Vin           = return $ string8 "vin"
render (LedMode m)   | m < (-1) || m > 3 = fail "LedMode out of range [-1,3]"
                     | otherwise         = return $ string8 "ledmode " <> intDec m
render List          = return $ string8 "list"
render ClearFaults   = return $ string8 "clear_faults"
render Coast         = return $ string8 "Coast" 
render Pwm           = return $ string8 "pwm"
render Off           = return $ string8 "off"
render On            = return $ string8 "on"
render (Pid params)  = return $ string8 "pid " <> renderPid params
render (Set point)   = return $ string8 "set " <> renderSet point
render (Bias b)      = return $ string8 "bias " <> floatDec b
render (PLimit l)    = return $ string8 "plimit " <> floatDec l
render (Select s)    = return $ string8 "select " <> renderSel s
render (Selonce s)   = return $ string8 "selonce " <> renderSel s
render (Combi m lst) = return $ string8 "combi " <> intDec m <> mconcat (map list lst)
  where list (x,y) = char8 ' ' <> intDec x <> char8 ' '  <> intDec y
render (Write1 bs)   = return $ string8 "write1 " <> mapSep (char8 ' ') word8HexFixed bs
render (Write2 bs)   = return $ string8 "write2 " <> mapSep (char8 ' ') word8HexFixed bs
render (e1 :> e2)    = liftA2 (\x y -> x <> string8 " ; " <> y) (render e1) (render e2)

renderPid :: PidParams -> Builder
renderPid (PidParams {..}) = mapSep (char8 ' ') id
                                [ intDec pvport
                                , intDec pvmode
                                , renderFormat pvformat
                                , floatDec pvscale
                                , intDec pvunwrap
                                , floatDec kp
                                , floatDec ki
                                , floatDec kd
                                , floatDec windup
                                ]

renderSet (Val f) = floatDec f
renderSet (WaveForm w) = renderWave w

renderWave (Square   a b c d) = string8 "square "   <> mapSep (char8 ' ') floatDec [a,b,c,d]
renderWave (Sine     a b c d) = string8 "sine "     <> mapSep (char8 ' ') floatDec [a,b,c,d]
renderWave (Triangle a b c d) = string8 "triangle " <> mapSep (char8 ' ') floatDec [a,b,c,d]
renderWave (Pulse    a b c)   = string8 "pulse "    <> mapSep (char8 ' ') floatDec [a,b,c,0]
renderWave (Ramp     a b c)   = string8 "ramp "     <> mapSep (char8 ' ') floatDec [a,b,c,0]

renderSel Nothing             = mempty
renderSel (Just (SelOff i f)) = intDec i <> char8 ' ' <> renderFormat f

renderFormat (Format True  b) = char8 's' <> renderByteLen b 
renderFormat (Format False b) = char8 'u' <> renderByteLen b

renderByteLen B1 = char8 '1'
renderByteLen B2 = char8 '2'
renderByteLen B4 = char8 '4'

mapSep :: Builder -> (a -> Builder) -> [a] -> Builder
mapSep sep build = foldr1 (\x xs -> x <> sep <> xs) . map build

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
