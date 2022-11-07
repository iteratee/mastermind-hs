{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Network.ListenThread
  ( listenAll,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
  ( Async (..),
    async,
    concurrently_,
    forConcurrently_,
    poll,
    pollSTM,
  )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan (..), newTChanIO, tryReadTChan, writeTChan)
import Data.ByteString (ByteString)
import Network.GuessThread
import System.Socket
  ( AddressInfo (..),
    AddressInfoFlags,
    Family (..),
    HasAddressInfo (..),
    Protocol (..),
    Socket (..),
    accept,
    bind,
    getAddressInfo,
    listen,
    socket,
  )
import System.Socket.Family.Inet (Inet (..))
import System.Socket.Family.Inet6 (Inet6 (..))
import System.Socket.Protocol.TCP (TCP (..))
import System.Socket.Type.Stream (Stream (..))

addrs6 :: [(ByteString, ByteString)]
addrs6 = [("::1", "12345")]

addrs4 = [("127.0.0.1", "12345")]

colors = 6

pegs = 4

-- Half second in microseconds
waitDelay = 5 * 100 * 1000

listenAll :: IO ()
listenAll = do
  chan <- newTChanIO
  concurrently_ (waitThread chan) $
    concurrently_ (listen6 chan) (listen4 chan)
  where
    listen4 chan =
      forConcurrently_ addrs4 $
        uncurry (listenThread @Inet @TCP chan)
    listen6 chan =
      forConcurrently_ addrs6 $
        uncurry (listenThread @Inet6 @TCP chan)

listenThread ::
  forall f p.
  (Family f, HasAddressInfo f, Protocol p, Show (SocketAddress f)) =>
  TChan (Async ()) ->
  ByteString ->
  ByteString ->
  IO ()
listenThread chan addrStr portStr = do
  infos <- getAddressInfo (Just addrStr) (Just portStr) mempty :: IO [AddressInfo f Stream p]
  forConcurrently_ infos $ \info -> do
    sock <- typedSocket
    bind sock (socketAddress info)
    listen sock 0
    listenThreadSocket chan sock
  where
    typedGetAddressInfo ::
      Maybe ByteString ->
      Maybe ByteString ->
      AddressInfoFlags ->
      IO [AddressInfo f Stream p]
    typedGetAddressInfo = getAddressInfo
    typedSocket :: IO (Socket f Stream p)
    typedSocket = socket

listenThreadSocket :: (Family f, Protocol p) => TChan (Async ()) -> Socket f Stream p -> IO ()
listenThreadSocket chan sock = do
  (connSock, _addr) <- accept sock
  newChild <- async (guessThreadPickColors colors pegs connSock)
  atomically $ writeTChan chan newChild
  listenThreadSocket chan sock

waitThread :: TChan (Async ()) -> IO ()
waitThread chan =
  go [] []
  where
    go :: [Async ()] -> [Async ()] -> IO ()
    go polledChildren (unpolledChild : rest) = do
      pollResult <- poll unpolledChild
      case pollResult of
        Nothing -> go (unpolledChild : polledChildren) rest
        Just _ -> do
          go polledChildren rest
    go polledChildren [] = do
      pollResult <- atomically $ do
        readResult <- tryReadTChan chan
        case readResult of
          Just newChild -> (Just newChild,) <$> pollSTM newChild
          Nothing -> return (Nothing, Nothing)
      case pollResult of
        (Just newChild, Nothing) -> go (newChild : polledChildren) []
        (Just newChild, Just _) -> go polledChildren []
        (Nothing, _) -> do
          threadDelay waitDelay
          go [] (reverse polledChildren)
