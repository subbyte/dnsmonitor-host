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

data TRD = TRD
    { timestamp  :: T.Text  
    , recordtype :: T.Text  
    , domainname :: T.Text  
    }

data DomainProcState = DomainProcState
    { tldMap :: DomainHitMap -- top-level domain
    , sldMap :: DomainHitMap -- second-level domain
    , currentTLD :: Maybe T.Text -- Just TLD
    , currentSLD :: Maybe T.Text -- Just SLD
    , currentdomain :: T.Text -- entire domain
    }

type DomainHitMap = HashMap.HashMap T.Text Integer

emptyDHM = DomainProcState HashMap.empty HashMap.empty Nothing Nothing ""

monitor :: IO ()
monitor = do
    -- DNS data Producer from tcpdump
    (_, Just hout, _, _) <- createProcess
        (proc "tcpdump" ["-i", "any", "-l", "-nn", "dst port 53"])
        { std_out = CreatePipe }
    -- streaming DNS data, process it, and print it
    runStream $ printS $ analyzeS $ rtFilter $ genTRDs $ S.fromHandle hout
  where
    genTRDs    = filterJust . S.mapM generateTRD
    rtFilter   = S.filter $ monitoredRecType . domainname
    analyzeS   = filterJust . S.scanx updateHitMap emptyDHM outputLine
    printS     = S.mapM TIO.putStrLn
    filterJust = fmap fromJust . S.filter isJust

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

updateHitMap :: DomainProcState -> TRD -> DomainProcState
updateHitMap m trd = DomainProcState tldM sldM tld sld dm
  where
    dm   = domainname trd
    dx   = splitOnDot dm
    tld  = Just . last $ init dx
    sld  = if length dx > 2 then Just . last . init $ init dx else Nothing
    tldM = incrementMap (tldMap m) tld
    sldM = incrementMap (sldMap m) sld

incrementMap :: DomainHitMap
             -> Maybe T.Text -- domain key if extracted
             -> DomainHitMap
incrementMap m Nothing  = m
incrementMap m (Just k) = HashMap.insertWith (+) k 1 m

outputLine :: DomainProcState -> Maybe T.Text
-- found SLD
outputLine (DomainProcState _    sldM _          (Just sld) dm) = Just $
    prepareOutputLine sld (sldM HashMap.! sld) dm
-- at least found TLD
outputLine (DomainProcState tldM _    (Just tld) Nothing    dm) = Just $
    prepareOutputLine tld (tldM HashMap.! tld) dm
-- no TLD (domainname == "."), the upstream should already yield an error
outputLine (DomainProcState tldM _    Nothing    Nothing    dm) = Nothing

prepareOutputLine :: T.Text -- domain key: TLD or SLD
                  -> Integer -- hit
                  -> T.Text -- full domain name
                  -> T.Text -- output line
prepareOutputLine dk hit dm = T.concat [color co $ padding dk, "| ", dm]
  where
    co = colorHit hit
    pos = T.length dk - displayKeySpan
    padding x = T.justifyLeft displayKeySpan ' '
              $ if pos > 0 then T.concat [T.dropEnd (pos + 4) x, "..."] else x

printErr :: T.Text -> IO ()
printErr = TIO.hPutStrLn stderr

printErrInvalidRec :: T.Text -> IO ()
printErrInvalidRec line = printErr $ T.concat ["[ERROR] invalid rec: ", line]
