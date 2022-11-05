{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    race,
  )
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

-- Five Seconds in microseconds
listenDelay = 5 * 1000 * 1000

listenAll :: IO ()
listenAll = do
  concurrently_ listen6 listen4
  where
    listen4 = forConcurrently_ addrs4 $ \(addr, port) ->
      listenThread @Inet @TCP addr port
    listen6 = forConcurrently_ addrs6 $ \(addr, port) ->
      listenThread @Inet6 @TCP addr port

listenThread ::
  forall f p.
  (Family f, HasAddressInfo f, Protocol p, Show (SocketAddress f)) =>
  ByteString ->
  ByteString ->
  IO ()
listenThread addrStr portStr = do
  infos <- getAddressInfo (Just addrStr) (Just portStr) mempty :: IO [AddressInfo f Stream p]
  forConcurrently_ infos $ \info -> do
    sock <- typedSocket
    bind sock (socketAddress info)
    listen sock 0
    listenThreadSocket sock
  where
    typedGetAddressInfo ::
      Maybe ByteString ->
      Maybe ByteString ->
      AddressInfoFlags ->
      IO [AddressInfo f Stream p]
    typedGetAddressInfo = getAddressInfo
    typedSocket :: IO (Socket f Stream p)
    typedSocket = socket

listenThreadSocket :: (Family f, Protocol p) => Socket f Stream p -> IO ()
listenThreadSocket sock = do
  go [] []
  where
    go :: [Async ()] -> [Async ()] -> IO ()
    go polledChildren (unpolledChild : rest) = do
      pollResult <- poll unpolledChild
      case pollResult of
        Nothing -> go (unpolledChild : polledChildren) rest
        Just _ -> go polledChildren rest
    go polledChildren [] = do
      raceResult <- race (threadDelay listenDelay) (accept sock)
      case raceResult of
        Left () -> go [] polledChildren
        Right (connSock, _addr) -> do
          newChild <- async (guessThreadPickColors colors pegs connSock)
          go [] (newChild : polledChildren)
