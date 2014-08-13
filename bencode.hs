{-# LANGUAGE OverloadedStrings #-}
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

type BKey = String
data BVal = BInt Integer
    | BStr String
    | BList [BVal]
    | BDict (M.Map BKey BVal)

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

main = C.putStrLn . bshow $ BDict $ M.fromList [("c", BInt 7), ("e", BInt 7), ("f", BList [BInt 3, BInt 4])]
