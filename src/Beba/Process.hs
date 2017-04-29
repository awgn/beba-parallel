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
    (proc ofProtocol
            [ "tcp:127.0.0.1:" ++ show (basePort + idx)
            , "tcp:" ++ controllerHost ++ ":" ++ show controllerPort
            , "--log-file=/var/log/ofprotocol.log." ++ show idx
            , "--verbose=ANY:ANY:info"
            , "--verbose=ANY:console:emer"
            ]) {
                 close_fds = True
               , delegate_ctlc = True
               }

mkOfDataPath idx Options{..} =
    (proc ofDataPath
            [   "ptcp:" ++ show (basePort + idx)
              , "-C", show (baseCore + idx)
              , "--no-slicing"
              , "-d", datapath_id idx
              , "--interfaces=" ++ intercalate "," interface
              , "--verbose=ANY:ANY:emer"
            ]) {
                 close_fds = True
               , delegate_ctlc = True
               }


datapath_id :: Int -> String
datapath_id n = let h = showHex (n+1) "" in
    take (12 - length h) "000000000000" ++ h

