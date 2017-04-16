module Beba.Options
    (
      Options(..)
    , parseOptions
    ) where

import Options.Applicative
import Data.Semigroup ((<>))


data Options = Options
    {
      controllerHost  :: String
    , controllerPort  :: Int
    , interface       :: [String]
    , baseCore        :: Int
    , basePort        :: Int
    , instances       :: Int

    , envCaplen       :: Int
    , envRxSlot       :: Int
    , envTxSlot       :: Int
    , envTxSync       :: Int

    , verbose         :: Bool
    , version         :: Bool

    } deriving (Show)



parseOptions :: Parser Options
parseOptions = Options

     <$> strOption
            ( long "contoller-host"
           <> short 'H'
           <> metavar "HOST"
           <> value "127.0.0.1"
           <> help "Specify the OF controller host")

     <*> option auto
            ( long "contoller-port"
           <> short 'P'
           <> metavar "PORT"
           <> value 6633
           <> help "Specify the OF controller port")

     <*> many (strOption
            ( long "interface"
           <> short 'i'
           <> metavar "IFNAME"
           <> help "Specify switch ports"))

     <*> option auto
            ( long "first-core"
           <> short 'c'
           <> metavar "CORE"
           <> showDefault
           <> value 0
           <> help "Index of the first core")

     <*> option auto
            ( long "first-port"
           <> short 'p'
           <> metavar "PORT"
           <> showDefault
           <> value 8000
           <> help "First TCP/port toward ofprotocol")

     <*> option auto
            ( long "instance"
           <> short 'j'
           <> metavar "NUM"
           <> showDefault
           <> value 1
           <> help "Number of ofdatapath")

     <*> option auto
            ( long "socket-caplen"
           <> metavar "BYTES"
           <> showDefault
           <> value 1500
           <> help "Socket environment option (PFQ_CAPLEN)")

     <*> option auto
            ( long "socket-rx-slots"
           <> metavar "INT"
           <> showDefault
           <> value 4096
           <> help "Socket environment option (PFQ_RX_SLOTS)")

     <*> option auto
            ( long "socket-tx-slots"
           <> metavar "INT"
           <> showDefault
           <> value 4096
           <> help "Socket environment option (PFQ_TX_SLOTS)")

     <*> option auto
            ( long "socket-tx-sync"
           <> metavar "INT"
           <> showDefault
           <> value 256
           <> help "Socket environment option (PFQ_TX_SYNC)")

     <*> switch
            ( long "verbose"
           <> short 'v'
           <> help "Enable verbose mode (debug)")

     <*> switch
            ( long "version"
           <> short 'V'
           <> help "Print version")


