{-# LANGUAGE CPP #-}
module Tools.TimePlot.Conf (
    ConcreteConf(..),
    Conf,
    readConf
) where

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString
import Data.Time hiding (parseTime)
import Data.Time.Parse
import Data.List
import Graphics.Rendering.Chart
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double

import Unsafe.Coerce

import Tools.TimePlot.Types

data ConcreteConf t =
  ConcreteConf {
    inFile        :: !FilePath,
    parseTime     :: !(B.ByteString -> Maybe (t, B.ByteString)),
    -- Input track -> (chart kind, suffix to append to track name for N:1 out:in mapping)
    chartKindF    :: !(S.ByteString -> [(ChartKind t, S.ByteString)]),

    fromTime      :: !(Maybe t),
    toTime        :: !(Maybe t),
    transformLabel :: !(t -> String -> String),

    outFile       :: !FilePath,
    outFormat     :: !OutFormat,
    outResolution :: !(Int,Int)
  }

type Conf = ConcreteConf LocalTime

data KindChoiceOperator = Cut | Accumulate

readConf :: [String] -> Conf
readConf args = readConf' parseTime 
  where
    pattern = case (words $ single "time format" "-tf" ("%Y-%m-%d %H:%M:%OS")) of
        "date":f -> B.pack (unwords f)
        f        -> B.pack (unwords f)
    Just (ourBaseTime,_) = strptime "%Y-%m-%d %H:%M:%OS" "1900-01-01 00:00:00" 
    {-# NOINLINE ourStrptime #-}
    ourStrptime :: B.ByteString -> Maybe (LocalTime, B.ByteString)
    ourStrptime = if pattern == B.pack "elapsed" 
                    then \s -> do
                      (d, s') <- readDouble s
                      return (fromSeconds d ourBaseTime `add` ourBaseTime, s')
                    else strptime pattern
    parseTime s = ourStrptime s

    int2double = fromIntegral :: Int -> Double
    single desc name def = case (getArg name 1 args) of
      [[r]] -> r
      []    -> def
      _     -> error $ "Single argument expected for: "++desc++" ("++name++")"

    readConf' :: (B.ByteString -> Maybe (LocalTime, B.ByteString)) -> ConcreteConf LocalTime
    readConf' parseTime = ConcreteConf {inFile=inFile, outFile=outFile, outFormat=outFormat, outResolution=outRes,
                      chartKindF=chartKindF, parseTime=parseTime, fromTime=fromTime, toTime=toTime,
                      transformLabel=transformLabel}
      where
        inFile      = single "input file"  "-if" (error "No input file (-if) specified")
        outFile     = single "output file" "-o"  (error "No output file (-o) specified")
        outFormat   = maybe PNG id $ lookup (single "output format" "-of" (name2format outFile)) $
            [("png",PNG), ("pdf",PDF), ("ps",PS), ("svg",SVG)]
          where
            name2format = reverse . takeWhile (/='.') . reverse
        outRes      = parseRes $ single "output resolution" "-or" "640x480"
          where
            parseRes s = case break (=='x') s of (h,_:v) -> (read h,read v)
        forceList :: [a] -> ()
        forceList = foldr seq ()
        chartKindF  = forceList [forceList plusKinds, forceList minusKinds, forceList defaultKindsPlus, defaultKindMinus `seq` ()] `seq` kindByRegex $
            [(Cut,        matches regex, parseKind0 (words kind)) | [regex,kind] <- getArg "-k" 2 args] ++
            [(Accumulate, matches regex, parseKind0 (words kind)) | [regex,kind] <- getArg "+k" 2 args]
          where
            plusKinds  = [parseKind0 (words kind) | [regex, kind] <- getArg "+k" 2 args]
            minusKinds = [parseKind0 (words kind) | [regex, kind] <- getArg "-k" 2 args]
            kindByRegex rks s = if null specifiedKinds then [defaultKindMinus] else specifiedKinds
              where
                specifiedKinds = defaultKindsPlus ++
                                 [k | (Accumulate, p, k) <- rks, p s] ++
                                 case [k | (Cut, p, k) <- rks, p s] of {k:_ -> [k]; _ -> []}
            matches regex = matchTest (makeRegexOpts defaultCompOpt (ExecOption {captureGroups = False}) regex)

        fromTime    = fst `fmap` (parseTime . B.pack $ single "minimum time (inclusive)" "-fromTime" "")
        toTime      = fst `fmap` (parseTime . B.pack $ single "maximum time (exclusive)" "-toTime"   "")
        baseTime    = if pattern == B.pack "elapsed"
                        then Just ourBaseTime
                        else (fst `fmap` (parseTime . B.pack $ single "base time"                "-baseTime" ""))

        transformLabel t s = case baseTime of
          Nothing -> s
          Just bt -> showDelta t bt

        parseKind0 (('+':suffix):k) = (parseKind k, S.pack "." `S.append` S.pack suffix)
        parseKind0 k                = (parseKind k, S.empty)

        parseKind :: [String] -> ChartKind LocalTime
        parseKind ["acount",  n  ] = KindACount    {binSize=read n}
        parseKind ("acount":_)     = error "acount requires a single numeric argument, bin size, e.g.: -dk 'acount 1'"
        parseKind ["apercent",n,b] = KindAPercent  {binSize=read n,baseCount=read b}
        parseKind ("apercent":_)   = error "apercent requires two numeric arguments: bin size and base value, e.g.: -dk 'apercent 1 480'"
        parseKind ["afreq",   n  ] = KindAFreq     {binSize=read n}
        parseKind ("afreq":_)      = error "afreq requires a single numeric argument, bin size, e.g.: -dk 'afreq 1'"
        parseKind ["freq",    n  ] = KindFreq      {binSize=read n,style=BarsStacked}
        parseKind ["freq",    n,s] = KindFreq      {binSize=read n,style=parseStyle s}
        parseKind ("freq":_)       = error $ "freq requires a single numeric argument, bin size, e.g.: -dk 'freq 1', " ++ 
                                             "or two arguments, e.g.: -dk 'freq 1 clustered'"
        parseKind ["hist",    n  ] = KindHistogram {binSize=read n,style=BarsStacked}
        parseKind ["hist",    n,s] = KindHistogram {binSize=read n,style=parseStyle s}
        parseKind ("hist":_)       = error $ "hist requires a single numeric argument, bin size, e.g.: -dk 'hist 1', " ++ 
                                             "or two arguments, e.g.: -dk 'hist 1 clustered'"
        parseKind ["event"       ] = KindEvent
        parseKind ("event":_)      = error "event requires no arguments"
        parseKind ["quantile",b,q] = KindQuantile  {binSize=read b, quantiles=read ("["++q++"]")}
        parseKind ("quantile":_)   = error $ "quantile requres two arguments: bin size and comma-separated " ++ 
                                             "(without spaces!) quantiles, e.g.: -dk 'quantile 1 0.5,0.75,0.9'"
        parseKind ["binf",    b,q] = KindBinFreq   {binSize=read b, delims   =read ("["++q++"]")}
        parseKind ("binf":_)       = error $ "binf requres two arguments: bin size and comma-separated " ++ 
                                             "(without spaces!) threshold values, e.g.: -dk 'binf 1 10,50,100,200,500'"
        parseKind ["binh",    b,q] = KindBinHist   {binSize=read b, delims   =read ("["++q++"]")}
        parseKind ("binh":_)       = error $ "binh requres two arguments: bin size and comma-separated " ++ 
                                             "(without spaces!) threshold values, e.g.: -dk 'binh 1 10,50,100,200,500'"
        parseKind ["lines"       ] = KindLines
        parseKind ("lines":_)      = error "lines requires no arguments"
        parseKind ["dots"        ] = KindDots { alpha = 1 }
        parseKind ["dots",    a  ] = KindDots { alpha = read a }
        parseKind ("dots":_)       = error "dots requires 0 or 1 arguments (the argument is alpha value: 0 = transparent, 1 = opaque, default 1)"
        parseKind ["cumsum",  b  ] = KindCumSum    {binSize=read b, subtrackStyle=SumStacked}
        parseKind ["cumsum",  b,s] = KindCumSum    {binSize=read b, subtrackStyle=parseSubtrackStyle s}
        parseKind ("cumsum":_)     = error $ "cumsum requires 1 or 2 arguments (bin size and subtrack style), e.g.: " ++ 
                                             "-dk 'cumsum 10' or -dk 'cumsum 10 stacked'"
        parseKind ["sum",     b  ] = KindSum       {binSize=read b, subtrackStyle=SumStacked}
        parseKind ["sum",     b,s] = KindSum       {binSize=read b, subtrackStyle=parseSubtrackStyle s}
        parseKind ("sum":_)        = error $ "sum requires one or two arguments: bin size and optionally " ++ 
                                             "subtrack style, e.g.: -dk 'sum 1' or -dk 'sum 1 stacked'"
        parseKind ("duration":ws)  = KindDuration  {subKind=parseKind ws}
        parseKind (('w':'i':'t':'h':'i':'n':'[':sep:"]"):ws)
                                   = KindWithin    {subKind=parseKind ws, mapName = fst . S.break (==sep)}
        parseKind ["none"        ] = KindNone
        parseKind ("none":_)       = error "none requires no arguments"
        parseKind ["unspecified" ] = KindUnspecified
        parseKind ("unspecified":_)= error "unspecified requires no arguments"
        parseKind ws               = error ("Unknown diagram kind " ++ unwords ws)

        defaultKindMinus = parseKind0 $ words $ single "default kind" "-dk" "unspecified"
        defaultKindsPlus = map (parseKind0 . words . head) $ getArg "+dk" 1 args

        parseStyle "stacked"   = BarsStacked
        parseStyle "clustered" = BarsClustered

        parseSubtrackStyle "stacked"   = SumStacked
        parseSubtrackStyle "overlayed" = SumOverlayed


-- getArg "-a" 2 ["-b", "1", "-a", "2", "q", "r", "-c", "3", "-a", "x"] =
-- [["2", "q"], ["x"]]
getArg :: String -> Int -> [String] -> [[String]]
getArg name arity args = [take arity as | (t:as) <- tails args, t==name]

