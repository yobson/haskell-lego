{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-| Description

This module deals with the serial communication
between haskell and the BuilHat. You can
use this to write raw commands to the pi hat
as well as initialising it and uploading firmware
-}
module Robotics.BuildHAT.LowLevel.Commands
( initialise
, uploadFirmware
, command
, Expr(..)
, PidParams(..)
, Format(..)
, BLen(..)
, SetPoint(..)
, Wave(..)
, SelMode(..)
, LedMode(..)
, Port(..)
) where


import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as B
import qualified System.Hardware.Serialport   as S
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Builder
import           System.Hardware.Serialport   hiding (send, recv)
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Data.Word
import           Data.Bits
import           Control.Monad
import           Control.Applicative

import Paths_buildhat

send s t = liftIO $ S.send s t

recv s l = liftIO $ S.recv s l

-- | BNF for BuildHAT Serial Protocol
data Expr = Echo !Bool   -- ^ Echo characters
          | Version      -- ^ Query Version
          | Port !Port    -- ^ Sets the current port, used implicitly by many other commands
          | Vin          -- ^ Prints the voltage present on the input power jack.
          | LedMode !LedMode -- ^ Sets the behaviour of the HAT’s LEDs.
          | List         -- ^ Prints a list of all the information known about the LPF2 devices connected to the HAT
          | ClearFaults  -- ^ Clears any latched motor power fault
          | Coast        -- ^ Switches the motor driver on the current port to 'coast' mode, that is, with both outputs floating.
          | Pwm          -- ^ Switches the controller on the current port to direct PWM mode.
          | On           -- ^ Same as `Pwm :> Set 0`
          | Off          -- ^ Same as `Pwm :> Set 1`
          | Pid !PidParams -- ^ Switches the controller on the current port to PID mode
          | Set !SetPoint  -- ^ Configures the setpoint for the controller on the current port
          | Bias !Float    -- ^ Sets a bias value for the current port which is added to positive motor drive values and subtracted from negative motor drive values
          | PLimit !Float  -- ^ Sets a global limit to the motor drive power on all ports
          | Select !(Maybe SelMode) -- ^ Selects the specified mode on the current port and repeatedly outputs that mode’s data as raw hexadecimal
          | Selonce !(Maybe SelMode) -- ^ As Select but outputs data once rather than repeatedly
          | Combi -- ^ Configures a combi mode on the current port
            !Int  -- ^ Port Index
            [(Int,Int)] -- ^ list of pairs of numbers, each pair giving a mode and an offset into that mode
          | Write1 ![Word8] -- ^ Writes the given hexadecimal bytes to the current port, the first byte being a header byte.
          | Write2 ![Word8] -- ^ Writes the given hexadecimal bytes to the current port, the first two bytes being header bytes.
          | !Expr :> !Expr  -- ^ One command followed by another
          deriving (Show)
infixr 5 :>

-- | The pidparams specify from where the process variable is to be
-- fetched and the gain coefficients for the controller itself
data PidParams  = PidParams
  { pvport   :: !Port    -- ^ port to fetch process variable from
  , pvmode   :: !Int    -- ^ mode to fetch process variable from
  , pvoffset :: !Int    -- ^ process variable byte offset into mode
  , pvformat :: !Format -- ^ process variable formay
  , pvscale  :: !Float  -- ^ process variable multiplicative scale factor
  , pvunwrap :: !Int    -- ^ 0=no unwrapping; otherwise modulo for process variable phase unwrap
  , kp       :: !Float  -- ^ proportional gain
  , ki       :: !Float  -- ^ integral gain
  , kd       :: !Float  -- ^ differential gain
  , windup   :: !Float  -- ^ integral windup limit
  } deriving (Show)

data Format = Format Bool -- ^ Signed?
                     BLen -- ^ Number of Bytes
  deriving Show

-- | Size (in bytes) of a number
data BLen = B1 -- ^ 1 byte
          | B2 -- ^ 2 bytes
          | B4 -- ^ 4 bytes
  deriving Show


data SetPoint = Val Float -- ^ Floating point constant
              | WaveForm Wave -- ^ Waveform
          deriving (Show)

data Wave = Square  -- ^ Square Wave
              Float -- ^ Min
              Float -- ^ Max
              Float -- ^ Period (seconds)
              Float -- ^ Initial Phase [0,1]
          | Sine    -- ^ Sine Wave
              Float -- ^ Min
              Float -- ^ Max
              Float -- ^ Period (seconds)
              Float -- ^ Initial Phase [0,1]
          | Triangle -- ^ Triangle Wave
              Float -- ^ Min
              Float -- ^ Max
              Float -- ^ Period (seconds)
              Float -- ^ Initial Phase [0,1]
          | Pulse   -- ^ Pulse
              Float -- ^ setpoint value during the pulse
              Float -- ^ setpoint value after the pulse
              Float -- ^ duration of the pulse
          | Ramp    -- ^ Ramp
              Float -- ^ setpoint value at the start of the ramp
              Float -- ^ setpoint value at the end of the ramp
              Float -- ^ duration of the ramp
          deriving (Show)

data SelMode = Sel 
                Int -- ^ Mode
             | SelOff 
                Int    -- ^ Mode
                Int    -- ^ Offset
                Format -- ^ Format
          deriving (Show)

data LedMode = LedV -- ^ LEDs lit depend on the voltage on the input power jack
             | LedOff  -- ^ LEDs off
             | LedOrange -- ^ orange
             | LedGreen -- ^ green
             | LedOG -- ^ orange and green together
  deriving Show

data Port = PortA
          | PortB
          | PortC
          | PortD
          deriving Show

command :: Expr -> BS.ByteString
command = BS.toStrict . toLazyByteString . (<> char8 '\r') . render

render :: Expr -> Builder
render (Echo True)   =  string8 "echo 1"
render (Echo False)  =  string8 "echo 0"
render Version       =  string8 "version"
render (Port p)      =  string8 "port " <> renderPort p
render Vin           =  string8 "vin"
render (LedMode m)   =  string8 "ledmode " <> renderLED m
render List          =  string8 "list"
render ClearFaults   =  string8 "clear_faults"
render Coast         =  string8 "Coast" 
render Pwm           =  string8 "pwm"
render Off           =  string8 "off"
render On            =  string8 "on"
render (Pid params)  =  string8 "pid " <> renderPid params
render (Set point)   =  string8 "set " <> renderSet point
render (Bias b)      =  string8 "bias " <> floatDec b
render (PLimit l)    =  string8 "plimit " <> floatDec l
render (Select s)    =  string8 "select " <> renderSel s
render (Selonce s)   =  string8 "selonce " <> renderSel s
render (Combi m lst) =  string8 "combi " <> intDec m <> mconcat (map list lst)
  where list (x,y) = char8 ' ' <> intDec x <> char8 ' '  <> intDec y
render (Write1 bs)   = string8 "write1 " <> mapSep (char8 ' ') word8HexFixed bs
render (Write2 bs)   = string8 "write2 " <> mapSep (char8 ' ') word8HexFixed bs
render (e1 :> e2)    = render e1 <> string8 " ; " <> render e2

renderLED LedV      = intDec (-1)
renderLED LedOff    = intDec 0
renderLED LedOrange = intDec 1
renderLED LedGreen  = intDec 2
renderLED LedOG     = intDec 3

renderPort PortA = intDec 0
renderPort PortB = intDec 1
renderPort PortC = intDec 2
renderPort PortD = intDec 3

renderPid :: PidParams -> Builder
renderPid (PidParams {..}) = mapSep (char8 ' ') id
                                [ renderPort pvport
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
renderSel (Just (SelOff i o f)) = intDec i <> char8 ' ' <> intDec o <> char8 ' ' <> renderFormat f

renderFormat (Format True  b) = char8 's' <> renderByteLen b 
renderFormat (Format False b) = char8 'u' <> renderByteLen b

renderByteLen B1 = char8 '1'
renderByteLen B2 = char8 '2'
renderByteLen B4 = char8 '4'

mapSep :: Builder -> (a -> Builder) -> [a] -> Builder
mapSep sep build = foldr1 (\x xs -> x <> sep <> xs) . map build

-- | Clear and initialise device
initialise :: (MonadIO m, MonadFail m) => SerialPort -> m ()
initialise s = do
  send s "\r"
  liftIO $ threadDelay 250000
  line <- recv s 2048
  send s "version\r"
  liftIO $ threadDelay 250000
  line <- recv s 1024
  when ("version\r\nBuildHAT bootloader" `B.isPrefixOf` line) $
    uploadFirmware s
  send s "echo 0\r"
  recv s 1024
  return ()

-- | Upload firmware if needed
uploadFirmware :: (MonadIO m, MonadFail m) => SerialPort -> m ()
uploadFirmware s = do
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
