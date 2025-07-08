module Gnosis.Ssh where

import           Control.Concurrent ( forkFinally, forkIO )
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

import           System.Console.ANSI
import           System.IO
import           System.Posix.Terminal
import           System.Posix.IO
import           System.Posix.Types       (Fd)
import           System.IO.Streams.Handle
import qualified System.IO.Streams        as Streams

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S
import qualified System.Socket.Unsafe           as S

import           Network.SSH
import qualified Network.SSH.Server   as Server
import qualified Network.SSH.Internal as SSHInternal

instance DuplexStream (S.Socket f S.Stream p) where

instance OutputStream (S.Socket f S.Stream p) where
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

getAppState :: Maybe (SSHInternal.TermInfo) -> AppState
getAppState (Just (SSHInternal.TermInfo pty)) =
  AppState { width  = fromEnum $ SSHInternal.ptyWidthCols pty
           , height = fromEnum $ SSHInternal.ptyHeightRows pty }
getAppState _ = AppState { width = 80, height = 22 }

strtobs :: String -> BS.ByteString
strtobs s = BS.pack $ fmap (fromIntegral . fromEnum) s

handleDirectTcpIpRequest :: identity -> Server.DirectTcpIpRequest -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest _idnt req = pure $ Just $ Server.DirectTcpIpHandler $ \stream-> do
    bs <- receive stream 4096
    sendAll stream "HTTP/1.1 200 OK\n"
    sendAll stream "Content-Type: text/plain\n\n"
    sendAll stream $! strtobs $ show req
    sendAll stream "\n\n"
    sendAll stream bs
    print bs

appLoop :: AppState -> Handle -> IO (ExitCode)
appLoop appState h = do
  c <- hGetChar h
  hSetCursorPosition h 4 5
  hClearLine h
  hSetSGR h [SetColor Foreground Vivid Yellow]
  hPutStr h ("You pressed: " ++ [c])
  hSetSGR h [Reset]
  hFlush h
  if c == 'q'
    then do
      hSetCursorPosition h 6 5
      hPutStrLn h "Exiting."
      pure ExitSuccess
    else appLoop appState h

handleSessionRequest :: identity -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest _idnt _req =
  pure $ Just $ Server.SessionHandler $
    \_env mterm _mcmd sin sout _stderr -> do
      let appState = getAppState mterm
      hIn  <- sshToIOSInput sin 4
      hOut <- sshToIOSOutput sout
      h <- streamPairToHandle hIn hOut
      hClearScreen h
      hSetCursorPosition h 2 5
      hSetSGR h [SetColor Foreground Vivid Cyan]
      hPutStr h "Press keys, q to quit"
      hSetSGR h [Reset]
      hFlush h
      appLoop appState h

sshToIOSOutput :: OutputStream sshOut => sshOut -> IO (Streams.OutputStream BS.ByteString)
sshToIOSOutput sshOut =
  Streams.makeOutputStream $ \mBs ->
    case mBs of
      Just bs -> void $ sendAll sshOut bs
      Nothing -> pure ()

sshToIOSInput :: InputStream sshIn => sshIn -> Int -> IO (Streams.InputStream BS.ByteString)
sshToIOSInput sshIn chunkSize =
  Streams.makeInputStream $ do
    bs <- receive sshIn chunkSize
    if BS.null bs
      then pure Nothing
      else pure (Just bs)

start :: IO ()
start = do
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
