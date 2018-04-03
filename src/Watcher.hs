{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Watcher
    ( watchSVGFiles
    ) where

import System.FSNotify
import Control.Monad (forever)
import Control.Concurrent (threadDelay, Chan)
import System.IO

watchSVGFiles :: String -> Chan Event -> Handle -> IO ()
watchSVGFiles d c lfh = withManager $ \mgr -> do
  stopWatcher <- watchDirChan mgr d (const True) c
  hPutStrLn lfh $ "started listener for folder: " ++ d
  forever $ threadDelay 1000000

