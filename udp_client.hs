import Network.Socket
import Data.List
import Text.Printf

pingQuery :: String
pingQuery = "d1:ad2:id20:1234579817508213921:q4:ping1:t2:aa1:y1:qe"

main :: IO()
main = do
    -- Prepare the socket
    addrInfos <- getAddrInfo Nothing (Just "weezy.us") (Just "1337")
    let addrInfo = head addrInfos
    let serverAddr = addrAddress addrInfo
    sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
     
    -- Send the query
    sent <- sendTo sock pingQuery serverAddr
    printf "sent \"%s\" (%u bytes) to %s\n" pingQuery sent (show serverAddr)

    -- Receive the response
    (buf, bufLen, recvAddr) <- recvFrom sock 1024
    printf "received \"%s\" (%u bytes) from %s\n" buf bufLen (show recvAddr)
