module WSConduit where

import           Conduit
import           Data.Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Conduit.List
import qualified Network.WebSockets
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           System.Process
import           GHC.IO.Handle
import           System.FSNotify
import           Control.Concurrent (Chan, readChan, threadDelay)

serviceConduit :: MonadIO m  => TVar T.Text -> Chan Event -> String -> String -> ConduitT TL.Text TL.Text m ()
serviceConduit oldsvg ch token d = mapMC $ \x -> case () of
  () | TL.unpack x == ("INIT" ++ token) -> liftIO $ do
    t <- TLIO.readFile (d ++ "/welcome.svg")
    atomically $ writeTVar oldsvg (TL.toStrict t)
    return (TL.append (TL.pack "FILE\n") t)
  () | TL.unpack x == ("OK" ++ token)   -> liftIO $ do
    putStrLn "-- waiting for event"
    ev <- readChan ch
    putStrLn $ show ev
    threadDelay 300000
    t <- TLIO.readFile (d ++ "/welcome.svg")
    seq (TL.length t) (return ())
    (Just hin, Just hout, Just herr, phand) <- createProcess (proc
                                                             "/usr/local/lib/amkhlv/compute-patch-to"
                                                             [d ++ "/welcome.svg"])
                                           {std_in = CreatePipe,
                                            std_out = CreatePipe,
                                            std_err = CreatePipe}
    old <- atomically $ do
      o <- readTVar oldsvg
      writeTVar oldsvg (TL.toStrict t)
      return o
    hPutStr hin (T.unpack old)
    hClose hin
    e <- hGetContents herr
    putStrLn e
    p <- hGetContents hout 
    putStrLn $ "PATCH>>>" ++ p ++ "<<<PATCH"
    hClose herr
    hClose hout
    waitForProcess phand
    return (TL.pack $ "PTCH\n" ++ p)
  other -> liftIO $ do
    putStrLn $ TL.unpack x
    putStrLn token
    return (TL.pack "FILE ERROR")
