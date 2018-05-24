{-# LANGUAGE OverloadedStrings #-}

module Monitor
( monitor
) where

import Config

import qualified Streamly.Prelude as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HashMap

import Streamly (runStream)
import Control.Monad (guard, when)
import Data.Maybe (isJust, isNothing, fromJust)
import System.IO (Handle, stderr)
import System.Process (createProcess, proc, std_out, StdStream(..))
import Data.HashMap.Strict ((!))

data TRD = TRD
    { timestamp  :: T.Text  
    , recordtype :: T.Text  
    , domainname :: T.Text  
    }

data DomainHitMap = DomainHitMap
    { tldMap :: HashMap.HashMap T.Text Integer -- top-level domain
    , sldMap :: HashMap.HashMap T.Text Integer -- second-level domain
    , currentTLD :: Maybe T.Text -- Just TLD
    , currentSLD :: Maybe T.Text -- Just SLD
    , currentdomain :: T.Text -- entire domain
    }

emptyDHM = DomainHitMap HashMap.empty HashMap.empty Nothing Nothing ""

monitor :: IO ()
monitor = do
    -- DNS data Producer from tcpdump
    (_, Just hout, _, _) <- createProcess
        (proc "tcpdump" ["-i", "any", "-l", "-nn", "dst port 53"])
        { std_out = CreatePipe }
    -- streaming DNS data, process it, and print it
    runStream $ print $ analyzeTRDs $ generateTRDs $ S.fromHandle hout
  where
    analyzeTRDs  = fmap fromJust . S.scanx updateHitMap emptyDHM outputLine
    generateTRDs = fmap fromJust . S.filter isJust . S.mapM generateTRD
    print        = S.mapM . TIO.putStrLn

generateTRD :: String -> IO (Maybe TRD)
generateTRD rawline = do
    when (isNothing mtrd) $ printErrInvalidRec line
    return mtrd
  where
    line = T.pack rawline
    mtrd = extractTRD line

extractTRD :: T.Text -> Maybe TRD
extractTRD line = do
    guard (length querysplits == 2)
    guard (length beforeQ == 7)
    guard (length afterQ == 3)
    guard (dm /= ".")
    return $ TRD ts rt dm
  where
    querysplits = T.split (=='?') line
    beforeQ     = splitOnSpace $ head querysplits
    afterQ      = splitOnSpace $ last querysplits
    rt          = last beforeQ
    ts          = head . splitOnDot $ head beforeQ
    dm          = head $ tail afterQ

splitOnSpace :: T.Text -> [T.Text]
splitOnSpace = T.split (== ' ')

splitOnDot :: T.Text -> [T.Text]
splitOnDot = T.split (== '.')

updateHitMap :: DomainHitMap -> TRD -> DomainHitMap
updateHitMap m trd = DomainHitMap tldM sldM tld sld dm
  where
    dm  = domainname trd
    dmItems = splitOnDot dm
    tld = Just . last $ init dmItems
    sld = if length dmItems > 2 then Just . last . init $ init dmItems else Nothing
    tldM = HashMap.insertWith (+) tld 1 $ tldMap m
    sldM = HashMap.insertWith (+) sld 1 $ sldMap m

outputLine :: DomainHitMap -> Maybe T.Text
-- found SLD
outputLine (DomainHitMap _    sldM _          (Just sld) dm) = prepareOutputLine tld (sldM ! sld) dm
-- at least found TLD
outputLine (DomainHitMap tldM _    (Just tld) Nothing    dm) = prepareOutputLine tld (tldM ! tld) dm
-- no TLD (domainname == "."), the upstream processor should already yield an error
outputLine (DomainHitMap tldM _    Nothing    Nothing    dm) = Nothing

prepareOutputLine :: T.Text -- domain key: TLD or SLD
                  -> Integer -- hit
                  -> T.Text -- full domain name
                  -> T.Text -- output line
prepareOutputLine dk hit dm = T.concat [color co $ padding dk, "| ", dm]
  where
    co = colorHit hit
    pos = T.length dk - displayKeySpan
    padding x = T.justifyLeft displayTLDSpan ' '
              $ if pos > 0 then T.concat [T.dropEnd (pos + 4) x, "..."] else x

printErr :: T.Text -> IO ()
printErr = TIO.hPutStrLn stderr

printErrInvalidRec :: T.Text -> IO ()
printErrInvalidRec line = printErr $
    T.concat ["[ERROR] invalid record: ", line]
