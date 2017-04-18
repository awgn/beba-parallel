{-# LANGUAGE RecordWildCards #-}

module Beba.Process
    ( mkOfProtocol
    , mkOfDataPath
    ) where


import Data.List (intercalate)
import System.Process
import Beba.Options
import Numeric (showHex)

mkOfProtocol ::  Int -> Options ->  CreateProcess
mkOfDataPath ::  Int -> Options ->  CreateProcess


mkOfProtocol idx Options{..} =
    (proc "/usr/local/bin/ofprotocol"
            [ "tcp:127.0.0.1:" ++ show (basePort + idx)
            , "tcp:" ++ controllerHost ++ ":" ++ show controllerPort
            , "--log-file=/var/log/ofprotocol.log." ++ show idx
            , "--verbose=ANY:console:emer"
            ]) {
                 close_fds = True
               , delegate_ctlc = True
               }

mkOfDataPath idx Options{..} =
    (proc "/usr/local/bin/ofdatapath"
            [   "ptcp:" ++ show (basePort + idx)
              , "-C", show (baseCore + idx)
              , "--no-slicing"
              , "-d", datapath_id idx
              , "--interfaces=" ++ intercalate "," interface
              , "--verbose=ANY:console:emer"
              , "--verbose=ANY:file:emer"
            ]) {
                 std_err = NoStream
               , close_fds = True
               , delegate_ctlc = True
               }


datapath_id :: Int -> String
datapath_id n = let h = showHex (n+1) "" in
    take (12 - length h) "000000000000" ++ h

