{-# LANGUAGE OverloadedStrings #-}

module Monitor
( monitor
) where

import Config

import qualified Streamly.Prelude as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HashMap

import Streamly (runStream, asyncly)
import Control.Monad (guard, when)
import Data.Maybe (isJust, isNothing, fromJust)
import System.IO (Handle, stderr)
import System.Process (createProcess, proc, std_out, std_err, StdStream(..))

data TRD = TRD
    { timestamp  :: T.Text  
    , recordtype :: T.Text  
    , domainname :: T.Text  
    }

data RecTypeFilterState = RTFS
    { prevDomain :: T.Text
    , currTRD :: TRD
    }

data DomainProcState = DPS
    { tldMap :: DomainHitMap -- top-level domain
    , sldMap :: DomainHitMap -- second-level domain
    , currentTLD :: Maybe T.Text -- Just TLD
    , currentSLD :: Maybe T.Text -- Just SLD
    , currentdomain :: T.Text -- entire domain
    }

type DomainHitMap = HashMap.HashMap T.Text Integer

emptyDHM = DPS HashMap.empty HashMap.empty Nothing Nothing ""
emptyRTFS = RTFS "" $ TRD "" "" ""

monitor :: IO ()
monitor = do
    -- DNS data Producer from tcpdump
    (_, Just hout, Just herr, _) <- createProcess
        (proc "tcpdump" ["-i", "any", "-l", "-nn", "dst port 53"])
        { std_out = CreatePipe
        , std_err = CreatePipe }
    -- streaming DNS data, process it, and print it
    runStream . asyncly
        $  (printS . analyzeS . dupF . rtF . genTRDs $ S.fromHandle hout)
        <> (S.mapM (printErr . T.pack) $ S.fromHandle herr)
  where
    -- generate TRD from raw tcpdump records
    genTRDs    = filterJust . S.mapM generateTRD
    -- filter out TRDs based on record types
    rtF        = S.filter $ monitoredRecType . domainname
    -- filter out duplicate TRDs which share same domain names
    dupF       = filterJust . S.scanx updateRTFS emptyRTFS eliminateTRD
    -- stateful analysis to output line for print
    analyzeS   = filterJust . S.scanx updateHitMap emptyDHM outputLine
    -- display line
    printS     = S.mapM TIO.putStrLn
    -- helper function with type (Maybe a) -> a
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

updateRTFS :: RecTypeFilterState -> TRD -> RecTypeFilterState
updateRTFS (RTFS _ (TRD _ _ dm)) trd = RTFS dm trd

eliminateTRD :: RecTypeFilterState -> Maybe TRD
eliminateTRD (RTFS dm' trd@(TRD _ _ dm)) =
    if dm == dm' then Nothing else Just trd

updateHitMap :: DomainProcState -> TRD -> DomainProcState
updateHitMap s trd = DPS tldM sldM tld sld dm
  where
    dm   = domainname trd
    dx   = splitOnDot dm
    tld  = Just . last $ init dx
    sld  = if length dx > 2 then Just . last . init $ init dx else Nothing
    tldM = incrementMap (tldMap s) tld
    sldM = incrementMap (sldMap s) sld

incrementMap :: DomainHitMap
             -> Maybe T.Text -- domain key if extracted
             -> DomainHitMap
incrementMap m Nothing  = m
incrementMap m (Just k) = HashMap.insertWith (+) k 1 m

outputLine :: DomainProcState -> Maybe T.Text
-- found SLD
outputLine (DPS _    sldM _          (Just sld) dm) = Just $
    prepareOutputLine sld (sldM HashMap.! sld) dm
-- at least found TLD
outputLine (DPS tldM _    (Just tld) Nothing    dm) = Just $
    prepareOutputLine tld (tldM HashMap.! tld) dm
-- no TLD (domainname == "."), the upstream should already yield an error
outputLine (DPS tldM _    Nothing    Nothing    dm) = Nothing

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

color :: T.Text -- what color to use
      -> T.Text -- the input
      -> T.Text -- the output
color co t = T.concat [colorCode co, t, colorCode "default"]

printErr :: T.Text -> IO ()
printErr msg = TIO.hPutStrLn stderr $ T.concat
    [if isErrMsg msg then color "red" "[error] " else "[message] ", msg]

printErrInvalidRec :: T.Text -> IO ()
printErrInvalidRec line = printErr $ T.concat ["invalid record: ", line]
