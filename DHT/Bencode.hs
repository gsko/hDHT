module DHT.Bencode (
    BVal(BInt, BStr, BList, BDict)
  , bencode
  , bdecode
  , (~~))
where

import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

data BVal = BInt Integer
    | BStr B.ByteString
    | BList [BVal]
    | BDict (M.Map BVal BVal)
    deriving (Show,Ord,Eq)

(~~) :: B.ByteString -> B.ByteString -> B.ByteString
(~~) = B.append

bencode :: BVal -> B.ByteString
bencode (BStr s) = blen ~~ ":" ~~ s
    where blen :: B.ByteString
          blen = C.pack . show . B.length $ s
bencode (BInt i) = C.pack $ printf "i%de" i
bencode (BList bs) = "l" ~~ foldl f "" bs ~~ "e"
    where f :: B.ByteString -> BVal -> B.ByteString
          f acc bval = acc ~~ bencode bval
bencode (BDict map) = "d" ~~ M.foldlWithKey f "" map ~~ "e"
    where f :: B.ByteString -> BVal -> BVal -> B.ByteString
          f acc key bval = acc ~~ bencode key ~~ bencode bval

bdecode :: B.ByteString -> Maybe [BVal]
bdecode = maybeResult . (parse . many $ bparse)

----- Parsers
bstr :: Parser BVal
bstr = do
    len <- decimal
    char ':'
    contents <- count (fromIntegral len) anyWord8
    return . BStr . B.pack $ contents
bint :: Parser BVal
bint = do
    char 'i'
    num <- decimal
    char 'e'
    return $ BInt num
blist :: Parser BVal
blist = do
    char 'l'
    xs <- many bparse
    char 'e'
    (return . BList) xs
bdict :: Parser BVal
bdict = do
    char 'd'
    kvPairs <- many1 bDictEntry
    char 'e'
    (return . BDict . M.fromList) kvPairs
bDictEntry :: Parser (BVal, BVal)
bDictEntry = do
    k <- bstr <?> "dict keys must be strings"
    v <- bparse
    return (k,v)
bparse :: Parser BVal
bparse = bint <|> bstr <|> blist <|> bdict
