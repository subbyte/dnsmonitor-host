{-# LANGUAGE OverloadedStrings #-}

module Config
( monitoredRecType
, displayKeySpan
, colorHit
, colorCode
, isErrMsg
) where

import Data.Text (Text, concat)
import qualified Data.Text as T

monitoredRecType :: Text -> Bool
monitoredRecType rt = notElem rt ["PTR"]

displayKeySpan :: Int
displayKeySpan = 20

-- display color when hit more than
colorHit :: Integer -> Text
colorHit h
    | h == 1    = "yellow"
    | h < 16    = "default"
    | h < 128   = "grey"
    | otherwise = "black"

colorCode :: Text -> Text
colorCode "black"   = "\x1b[30m"
colorCode "red"     = "\x1b[31m"
colorCode "green"   = "\x1b[32m"
colorCode "yellow"  = "\x1b[33m"
colorCode "blue"    = "\x1b[34m"
colorCode "magenta" = "\x1b[35m"
colorCode "cyan"    = "\x1b[36m"
colorCode "white"   = "\x1b[37m"
colorCode "grey"    = "\x1b[90m"
colorCode _         = "\x1b[0m"

isErrMsg :: Text -> Bool
isErrMsg m = and [null $ T.breakOnAll x m | x <- nonTcpdumpErrMsgNeedles]

nonTcpdumpErrMsgNeedles :: [Text]
nonTcpdumpErrMsgNeedles = ["verbose output suppressed", "listening on"]
