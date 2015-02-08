module Tools.TimePlot.Source (
    readSource
) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double
import Tools.TimePlot.Types

readSource :: (Show t) => (B.ByteString -> Maybe (t,B.ByteString)) -> FilePath -> IO (ParseResult t)
readSource readTime f = (toParseResult . map parseLine . filter (not . B.null) . blines) `fmap` 
                        (if f == "-" then B.getContents else B.readFile f)
  where
    blines   = map pruneLF . B.split '\n'
    pruneLF b | not (B.null b) && (B.last b == '\r') = B.init b
              | otherwise                            = b
    strict   = S.concat . B.toChunks
    toParseResult [] = ParseResult [] []
    toParseResult (Left e:es) = let ~(ParseResult pd up) = toParseResult es in ParseResult (e:pd) up
    toParseResult (Right s:es) = let ~(ParseResult pd up) = toParseResult es in ParseResult pd (s:up)
    parseLine s = (\x -> case x of { Just e -> Left e; Nothing -> Right (strict s) }) $ do
      (t, s') <- readTime s
      (_, s'') <- B.uncons s'
      (c,rest) <- B.uncons s''
      case c of
        '>' -> return (t, InEdge (strict rest) Rise )
        '<' -> return (t, InEdge (strict rest) Fall )
        '!' -> do
          let (track, val') = B.break (==' ') rest
          if B.null val'
            then return (t, InEdge (strict track) (Pulse (Status "" "")))
            else do
              (_,val) <- B.uncons val'
              return (t, InEdge (strict track) $ Pulse (Status "" (B.unpack val)))
        '@' -> do
          let (track, val') = B.break (==' ') rest
          (_,val) <- B.uncons val'
          return (t, InEdge (strict track) $ SetTo (Status {statusColor = B.unpack $ val, statusLabel = ""}))
        '=' -> do
          let (track, val') = B.break (==' ') rest
          (_,val) <- B.uncons val'
          if B.null val
            then Nothing
            else do
              case B.head val of
                '`' -> do
                  return (t, InAtom (strict track) (strict $ B.tail val))
                _   -> do
                  (v,_  ) <- readDouble val
                  return (t, InValue (strict track) v)
        _   -> Nothing

