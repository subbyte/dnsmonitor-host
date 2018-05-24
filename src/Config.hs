{-# LANGUAGE OverloadedStrings #-}

module Config
( color
, displayKeySpan
, colorHit
) where

import Data.Text (Text, concat)
import qualified Data.Text as T

displayKeySpan :: Int
displayKeySpan = 20

-- display color when hit more than
colorHit :: Integer -> Text
colorHit h = if h > 10 then "red" else "default"

color :: Text -- what color to use
      -> Text -- the input
      -> Text -- the output
color co t = T.concat [colorCode co, t, colorCode "default"]

colorCode :: Text -> Text
colorCode "black"   = "\x1b[30m"
colorCode "red"     = "\x1b[31m"
colorCode "green"   = "\x1b[32m"
colorCode "yellow"  = "\x1b[33m"
colorCode "blue"    = "\x1b[34m"
colorCode "magenta" = "\x1b[35m"
colorCode "cyan"    = "\x1b[36m"
colorCode "white"   = "\x1b[37m"
colorCode _         = "\x1b[0m"
