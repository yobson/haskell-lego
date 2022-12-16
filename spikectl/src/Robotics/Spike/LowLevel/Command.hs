{-# LANGUAGE OverloadedStrings #-}
{-| Description

This module provides an Embedded language for
the raw serial commands to control the spike board
as well as a function to render a command as a serial command
-}
module Robotics.Spike.LowLevel.Command
(-- * Types
  Command(..)
, Port(..)
, PortAction(..)
, MTCommand(..)
, DistCommand(..)
, DispCommand(..)
, MotionCommand(..)
  -- * Functions
, buildCommand
, chkError
, parseUnit
, parse3Tuple
) where

import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString                    as B
import           Data.ByteString.Builder
import           Control.Monad.IO.Class
import           Data.Word
import           Data.Attoparsec.ByteString.Char8
import           Control.Applicative
import           Data.Functor
import           System.Hardware.Serialport


buildCommand com = B.toStrict $ toLazyByteString $ renderCommand com <> string8 "\r\n"

-- | Port that a sensor or motor is connected to
data Port = PortA | PortB | PortC | PortD | PortE | PortF
  deriving (Show, Eq, Enum)

-- | Commands
data Command = PowerOff 
             | PortCom Port PortAction
             | Display DispCommand
             | Motion MotionCommand
  deriving (Show, Eq)

-- | Read or Write to a 'Port'
data PortAction = MAction MTCommand -- ^ Motor Action
                | DistAction DistCommand
  deriving (Show, Eq)

renderPort PortA = string8 "A."
renderPort PortB = string8 "B."
renderPort PortC = string8 "C."
renderPort PortD = string8 "D."
renderPort PortE = string8 "E."
renderPort PortF = string8 "F."

renderCommand PowerOff           = string8 "hub.power_off()"
renderCommand (PortCom port act) = string8 "hub.port." <> renderPort port <> renderPortAction act
renderCommand (Display act)      = string8 "hub.display." <> renderDispCommand act
renderCommand (Motion act)       = string8 "hub.motion." <> renderMotionCommand act

renderPortAction (MAction act)    = string8 "motor."  <> renderMTCommand act
renderPortAction (DistAction act) = string8 "device." <> renderDistCommand act

-- | Motor Commands
data MTCommand = SetPWM Int -- ^ Set Speed Raw
               | RunAtSpeed Int
               | RunToPos Int Int -- ^ Angle Speed
  deriving (Show, Eq)

renderMTCommand (SetPWM val) = string8 "pwm(" <> intDec val <> string8 ")"
renderMTCommand (RunAtSpeed speed) = string8 "run_at_speed(" <> intDec speed <> string8 ")"
renderMTCommand (RunToPos pos speed) = string8 "run_to_position(" <> intDec pos <> string8 "," <> intDec speed <> string8 ")"

data DistCommand = DistLED Word8 Word8 Word8 Word8
                 | DistLEDsOn
                 | DistLEDsOff
                 | DistGet
  deriving (Show, Eq)

renderDistCommand (DistLED tl tr bl br)  = string8 "mode(5, bytes([" <> renderSepBy (string8 ", ") [word8Dec tl, word8Dec tr, word8Dec bl, word8Dec br] <> string8 "]))"
renderDistCommand DistLEDsOn             = renderDistCommand $ DistLED 9 9 9 9
renderDistCommand DistLEDsOff            = renderDistCommand $ DistLED 0 0 0 0
renderDistCommand DistGet                = string8 "get()"


data DispCommand = Draw [[Word8]]
                 | Print String
                 | ClearScreen
  deriving (Show, Eq)

renderDispCommand (Draw pic)  = string8 "show(hub.Image(\"" <> matrixToImage pic <> string8 "\"))"
renderDispCommand (Print str) = string8 "show(\"" <> string8 str <> string8 "\")"
renderDispCommand ClearScreen = string8 "clear()"

matrixToImage :: [[Word8]] -> Builder
matrixToImage = renderSepBy (string8 ":") . map (foldr (\x xs -> word8Dec x <> xs) mempty)

data MotionCommand = Position
  deriving (Eq, Show)

renderMotionCommand Position = string8 "position()"

renderSepBy :: Builder -> [Builder] -> Builder
renderSepBy div = foldr1 (\x xs -> x <> div <> xs)


unitInt :: Parser Int
unitInt =  char '[' *> signed decimal <* char ']'
       <|> string "[]" $> 0
       <|> string "[None]" $> 0

tuple3 :: Parser (Int, Int, Int)
tuple3 = char '(' *> liftA3 (,,) (signed decimal <* char ',' <* skipSpace)
                                 (signed decimal <* char ',' <* skipSpace)
                                 (signed decimal <* skipSpace) <* char ')'

postamble :: B.ByteString -> Parser a -> Parser a
postamble com p = string com *> p <* endOfLine <* string ">>> "

buildComParser :: (MonadIO m, MonadFail m) => Parser a -> B.ByteString -> SerialPort -> m a
buildComParser p com s = do
  init <- liftIO $ recv s 1024
  res  <- parseWith (liftIO  getMore) (postamble com p) init
  either failure return $ eitherResult res
    where getMore = recv s 1024
          failure e = do
            liftIO $ recv s 1024
            fail e

-- | Parse a result which is a number in a signleton list
parseUnit :: (MonadIO m, MonadFail m) => B.ByteString -> SerialPort -> m Int
parseUnit = buildComParser unitInt

parse3Tuple :: (MonadIO m, MonadFail m) => B.ByteString -> SerialPort -> m (Int,Int,Int)
parse3Tuple = buildComParser tuple3

-- | Fail on serial error
chkError :: (MonadIO m, MonadFail m) => B.ByteString -> SerialPort -> m ()
chkError com = buildComParser (pure ()) $ B.dropEnd 2 com
