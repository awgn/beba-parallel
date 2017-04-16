{-# LANGUAGE RecordWildCards #-}

module Main where

import Options
import Options.Applicative
import Data.Semigroup ((<>))

bebaVer :: String
bebaVer = "0.1"

main :: IO ()
main = execParser opts >>= mainRun
    where
       opts = info (helper <*> parseOptions)
        (fullDesc <> header "beba-parallel: multi-core beba switch")


mainRun :: BebaOptions -> IO ()
mainRun opt@BebaOptions{..}
  | version        = putStrLn bebaVer
  | otherwise      = print opt
