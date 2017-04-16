{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Beba.Options as Beba
import Beba.Env

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import qualified Paths_beba_parallel as P


main :: IO ()
main = execParser opts >>= mainRun
    where
       opts = info (helper <*> Beba.parseOptions)
        (fullDesc <> header "beba-parallel: multi-core beba switch")


mainRun :: Beba.Options -> IO ()
mainRun opt@Beba.Options{..}
  | version        = putStrLn $ showVersion P.version
  | otherwise      = print opt >> print (pfq_socket_env opt 0)
