module Tools.TimePlot.Source (
    readSource
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Fractional
import Tools.TimePlot.Types

readSource :: (Show t) => (S.ByteString -> Maybe (t, S.ByteString)) -> FilePath -> IO (ParseResult t)
readSource readTime f = (toParseResult . map (parseLine . B.toStrict) . filter (not . B.null) . blines) `fmap` 
                        (if f == "-" then B.getContents else B.readFile f)
  where
    blines   = map pruneLF . B.split '\n'
    pruneLF b | not (B.null b) && (B.last b == '\r') = B.init b
              | otherwise                            = b
    toParseResult [] = ParseResult [] []
    toParseResult (Left e:es) = let ~(ParseResult pd up) = toParseResult es in ParseResult (e:pd) up
    toParseResult (Right s:es) = let ~(ParseResult pd up) = toParseResult es in ParseResult pd (s:up)
    parseLine s = (\x -> case x of { Just e -> Left e; Nothing -> Right s }) $ do
      (t, s') <- readTime s
      (_, s'') <- S.uncons s'
      (c,rest) <- S.uncons s''
      case c of
        '>' -> return (t, InEdge rest Rise )
        '<' -> return (t, InEdge rest Fall )
        '!' -> do
          let (track, val') = S.break (==' ') rest
          let label = S.unpack $ S.drop 1 val'
          return (t, InEdge track (Pulse (Status "" label)))
        '@' -> do
          let (track, val') = S.break (==' ') rest
          (_,val) <- S.uncons val'
          return (t, InEdge track $ SetTo (Status {statusColor = S.unpack $ val, statusLabel = ""}))
        '=' -> do
          let (track, val') = S.break (==' ') rest
          (_,val) <- S.uncons val'
          case S.uncons val of
            Nothing -> Nothing
            Just (v, val') -> case v of
              '`' -> do
                return (t, InAtom track val')
              _   -> do
                (v, _) <- readSigned readDecimal val
                return (t, InValue track v)
        _   -> Nothing

