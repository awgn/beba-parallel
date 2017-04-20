{-# LANGUAGE RecordWildCards #-}

module Beba.Env where

import Beba.Options
import Data.List (isPrefixOf)
import Data.Monoid

pfqEnvironment :: Int -> Options -> [(String, String)]
pfqEnvironment n Options{..} =
    [   ("LD_PRELOAD"       , "/usr/local/lib/libpcap.so")
      , ("PFQ_CAPLEN"       , show envCaplen)
      , ("PFQ_RX_SLOTS"     , show envRxSlot)
      , ("PFQ_TX_SLOTS"     , show envTxSlot)
      , ("PFQ_TX_SYNC"      , show envTxSync)
      , ("PFQ_TX_HW_QUEUE"  , show n)
    ] <> (pfqDevGroup <$> interface)
      <> (pfqFanout n envFanout)


pfqDevGroup :: String -> (String, String)
pfqDevGroup xs
  | "eth" `isPrefixOf` xs = ("PFQ_GROUP_" <> xs, drop 3 xs)
  | otherwise = ("PFQ_GROUP_" <> xs, "63")


pfqFanout :: Int -> Maybe String -> [(String, String)]
pfqFanout n steer | 
    n == 0, Just xs <- steer = [("PFQ_LANG", xs)]
pfqFanout _ _ = []
