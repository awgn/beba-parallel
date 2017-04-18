{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Beba.Options as Beba
import qualified Beba.Env     as Beba
import qualified Beba.Process as Beba

import Options.Applicative

import Data.Semigroup ((<>))
import Data.Version (showVersion)
import qualified Paths_beba_parallel as P

import System.Posix.Signals
import System.Environment
import System.Process
import System.IO

import Control.Monad (forM, when)

import Beba.Env

main :: IO ()
main = execParser opts >>= mainRun
    where
       opts = info (helper <*> Beba.parseOptions)
        (fullDesc <> header "beba-parallel: multi-core BEBA switch (github.com/beba-eu/beba-switch)")


mainRun :: Beba.Options -> IO ()
mainRun opt@Beba.Options{..}
  | version        = putStrLn $ showVersion P.version
  | otherwise      = do
      hld <- mainRunSwitch opt
      _ <- installHandler sigTERM (Catch $ routeSignal "TERM" hld) Nothing
      _ <- installHandler sigINT  (Catch $ routeSignal "INT"  hld) Nothing
      _ <- installHandler sigHUP  (Catch $ routeSignal "HUP"  hld) Nothing
      mainWait hld


routeSignal :: String -> [(ProcessHandle, ProcessHandle)] -> IO ()
routeSignal n hdl = do
    putStrLn $ "Signal " ++ n ++ " catched..."
    mapM_ (\(a,b) -> terminateProcess a >> terminateProcess b) hdl


mainWait :: [(ProcessHandle, ProcessHandle)] -> IO ()
mainWait = mapM_ (\(a,b) -> waitForProcess a >> waitForProcess b)


mainRunSwitch :: Beba.Options -> IO [(ProcessHandle, ProcessHandle)]
mainRunSwitch opt@Beba.Options{..} = do
    putStrLn $ "Launching " ++ show instances ++ " instances ofdatapath/ofprotocol..."
    forM [ 0 .. instances-1 ] $ \n -> do
        env' <- (<> pfqEnvironment n opt) <$> getEnvironment
        dpath <- openFile ("/var/log/ofdatapath.log." ++ show n) AppendMode
        (_, _, _, d) <- launchProcess verbose $ (Beba.mkOfDataPath n opt) {std_out = UseHandle dpath, env = Just env' }
        (_, _, _, p) <- launchProcess verbose $ Beba.mkOfProtocol n opt
        return (d,p)


launchProcess :: Bool -> CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchProcess verb cp = when verb (print cp) *> createProcess cp


mainDummy :: Beba.Options -> IO ()
mainDummy opt =
    putStrLn "Options:" *>
     print opt           *>
      putStrLn "Env:"     *>
       print (Beba.pfqEnvironment 0 opt)

