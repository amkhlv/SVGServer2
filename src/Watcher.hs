{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Watcher
    ( watchSVGFiles
    ) where

import System.FSNotify
import Control.Monad (forever)
import Control.Concurrent (threadDelay, Chan)

watchSVGFiles :: String -> Chan Event -> IO ()
watchSVGFiles d c = withManager $ \mgr -> do
  stopWatcher <- watchDirChan mgr d (const True) c
  putStrLn $ "started listener for folder: " ++ d
  forever $ threadDelay 1000000

