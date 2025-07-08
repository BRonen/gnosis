{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent ( forkFinally )
import           Control.Exception  ( bracket
                                    , bracketOnError
                                    , handle
                                    , throwIO )
import           Control.Monad      ( forever, void )
import qualified Data.ByteArray     as BA
import qualified Data.ByteString    as BS
import           Data.Word
import           Data.Default
import           System.Exit
import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S
import qualified System.Socket.Unsafe           as S

import           Network.SSH
import qualified Network.SSH.Server   as Server
import qualified Network.SSH.Internal as SSHInternal

instance DuplexStream (S.Socket f S.Stream p) where

instance OutputStream  (S.Socket f S.Stream p) where
    send stream bytes =
        handle f $ S.send stream bytes S.msgNoSignal
        where
            f e
                | e == S.ePipe = pure 0
                | otherwise    = throwIO e
    sendUnsafe stream (BA.MemView ptr n) = fromIntegral <$>
        handle f (S.unsafeSend stream ptr (fromIntegral n) S.msgNoSignal)
        where
            f e
                | e == S.ePipe = pure 0
                | otherwise    = throwIO e

instance InputStream  (S.Socket f S.Stream p) where
    peek stream len = S.receive stream len (S.msgNoSignal <> S.msgPeek)
    receive stream len = S.receive stream len S.msgNoSignal
    receiveUnsafe stream (BA.MemView ptr n) = fromIntegral <$>
        S.unsafeReceive stream ptr (fromIntegral n) S.msgNoSignal

data AppState = AppState { width  :: Int
                         , height :: Int }

clear :: AppState -> BS.ByteString
clear appState = BS.replicate n $ (fromIntegral (fromEnum '=') :: Word8)
  where n = (width appState) * (height appState)

appLoop :: InputStream stdin => OutputStream stdout => AppState -> stdin -> stdout -> IO (ExitCode)
appLoop appState stdin stdout = do
  bs <- receive stdin 1
  let h = BS.head bs
  putStrLn $ show h
  if (show h == "27")
  then pure ExitSuccess
  else do
    if (show h == "99")
    then do
      sendAll stdout $ clear appState
      appLoop appState stdin stdout
    else do
      sendAll stdout $ bs <> bs
      sendAll stdout "\n"
      appLoop appState stdin stdout

getAppState :: Maybe (SSHInternal.TermInfo) -> AppState
getAppState (Just (SSHInternal.TermInfo pty)) =
  AppState { width  = fromEnum $ SSHInternal.ptyWidthCols pty
           , height = fromEnum $ SSHInternal.ptyHeightRows pty }
getAppState _ = AppState { width = 80, height = 22 }

handleSessionRequest :: identity -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest _idnt _req =
  pure $ Just $ Server.SessionHandler $
    \_env mterm _mcmd stdin stdout _stderr -> do
      let appState = getAppState mterm
      sendAll stdout "Hello world!\n"
      appLoop appState stdin stdout

handleDirectTcpIpRequest :: identity -> Server.DirectTcpIpRequest -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest _idnt req = pure $ Just $ Server.DirectTcpIpHandler $ \stream-> do
    bs <- receive stream 4096
    sendAll stream "HTTP/1.1 200 OK\n"
    sendAll stream "Content-Type: text/plain\n\n"
    sendAll stream $! BS.pack $ fmap (fromIntegral . fromEnum) $ show req
    sendAll stream "\n\n"
    sendAll stream bs
    print bs

main :: IO ()
main = do
  file                <- BS.readFile "./resources/id_ed25519"
  (privateKey, _) : _ <- decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  let serverConfig = def
        { Server.transportConfig          = def
        , Server.userAuthConfig           = def
          { Server.onAuthRequest          = \username _ _ -> pure (Just username) }
          , Server.connectionConfig       = def
            { Server.onSessionRequest     = handleSessionRequest
            , Server.onDirectTcpIpRequest = handleDirectTcpIpRequest } }
  let open = S.socket :: IO (S.Socket S.Inet6 S.Stream S.Default)
  let close = S.close
  bracket open close (accept serverConfig privateKey)
  where
    accept config agent s = do
        S.setSocketOption s (S.ReuseAddress True)
        S.setSocketOption s (S.V6Only False)
        S.bind s (S.SocketAddressInet6 S.inet6Any 2222 0 0)
        S.listen s 5
        forever $ bracketOnError (S.accept s) (S.close . fst) $ \(stream, peer) -> do
            putStrLn $ "Connection from " ++ show peer
            void $ forkFinally
                (Server.serve config agent stream >>= print)
                (const $ S.close stream)
