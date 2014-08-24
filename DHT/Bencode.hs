{-# LANGUAGE OverloadedStrings #-}
module DHT.Bencode (BKey, BVal(BInt, BStr, BList, BDict), bshow) where

import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

type BKey = String
data BVal = BInt Integer
    | BStr String
    | BList [BVal]
    | BDict (M.Map BKey BVal)
    deriving Show

(~~) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(~~) = BS.append

bshow :: BVal -> BS.ByteString
bshow (BStr s) = C.pack $ printf "%d:%s" (length s) s
bshow (BInt i) = C.pack $ printf "i%de" i
bshow (BList bs) = "l" ~~ foldl f "" bs ~~ "e"
    where f :: BS.ByteString -> BVal -> BS.ByteString
          f acc b = acc ~~ bshow b
bshow (BDict map) = "d" ~~ M.foldlWithKey f "" map ~~ "e"
    where f :: BS.ByteString -> String -> BVal -> BS.ByteString
          f acc k b = acc ~~ bshow (BStr k) ~~ bshow b

----- Parsers
number :: Parser Integer
number =
    do neg <- try $ string "-" <|> string ""
       nStr <- many1 digit
       return $ read (neg ++ nStr)
bint :: Parser BVal
bint =
    do char 'i'
       num <- number
       char 'e'
       return $ BInt num
bdecode :: String -> Either ParseError [BVal]
bdecode = parse (many bint) ""

main = putStrLn . show $ bdecode "i5ei6e"
