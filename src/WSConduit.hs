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
import qualified Control.Monad as CM
import           System.Process
import           GHC.IO.Handle
import           System.FSNotify
import           Control.Concurrent (Chan, readChan, threadDelay)
import           System.IO


okLoop :: 
  TVar (Maybe FilePath) -> TVar T.Text -> Chan Event -> String -> Handle -> String -> 
  IO TL.Text
okLoop oldpathm oldsvg ch d lfh diff = liftIO $ do
  ev <- readChan ch
  hPutStrLn lfh ("-- detected event: " ++ show ev) >> hFlush lfh
  threadDelay 100000
  let fp = case ev of
        Added fp _ _ -> case reverse . take 4 $ reverse fp of
          ".svg" -> Just fp
          _ -> Nothing
        Modified fp _ _ -> case reverse . take 4 $ reverse fp of 
          ".svg" -> Just fp
          _ -> Nothing
        Removed fp _ _ -> Nothing
  case fp of
    Just filepath -> do
      oldFilePath <- atomically $ do
        o <- readTVar oldpathm
        writeTVar oldpathm (Just filepath)
        return o
      case oldFilePath of
        Just oldpath | oldpath == filepath -> do
          t <- TLIO.readFile filepath
          seq (TL.length t) (return ())
          (Just hin, Just hout, Just herr, phand) <- createProcess (proc diff [filepath])
                                                     {std_in = CreatePipe,
                                                      std_out = CreatePipe,
                                                      std_err = CreatePipe}
          hPutStr lfh "-- computing patch ... "
          oldContents <- atomically $ do
            o <- readTVar oldsvg
            writeTVar oldsvg (TL.toStrict t)
            return o
          hPutStr hin (T.unpack oldContents)
          hClose hin
          hFlush lfh
          p <- hGetContents hout
          if length p < 100 then hPutStrLn lfh p else hPutStrLn lfh ("PATCH size = " ++ show (length p))
          hFlush lfh
          hClose hout
          e <- hGetContents herr
          if not (null e) 
          then do
            hPutStrLn lfh "Diff error:"
            hPutStrLn lfh e 
            hPutStrLn lfh "Resending FILE"
            hFlush lfh
            hClose herr
            return (TL.append (TL.pack "FILE\n") t)            
          else do
            hFlush lfh
            hClose herr
            waitForProcess phand
            if null p 
            then okLoop oldpathm oldsvg ch d lfh diff 
            else do
              hPutStrLn lfh "-- transmitting patch, WAITING for receipt"
              hFlush lfh
              return (TL.pack $ if null p then "NOTH\n" else "PTCH\n" ++ p)
        _ -> do
          t <- TLIO.readFile filepath
          atomically $ writeTVar oldsvg (TL.toStrict t)
          return (TL.append (TL.pack "FILE\n") t)            
    Nothing -> return (TL.pack "NOTH\n")
  

serviceConduit :: MonadIO m  =>
  TVar (Maybe FilePath) -> TVar T.Text -> Chan Event -> String -> String -> Handle -> String -> 
  ConduitT TL.Text TL.Text m ()
serviceConduit oldpathm oldsvg ch token d lfh diff = mapMC $ \x -> case () of
  () | TL.unpack x == ("INIT" ++ token) -> liftIO $ return (TL.pack "NOTH\n")
  () | TL.unpack x == ("ERROR" ++ token) -> liftIO $ do
    hPutStrLn lfh "-- client detected ERROR, resending file --" >> hFlush lfh
    oldFilePath <- readTVarIO oldpathm
    case oldFilePath of
      Just ofp -> do
        hPutStrLn lfh "-- FILE --" >> hFlush lfh
        t <- TLIO.readFile ofp
        return (TL.append (TL.pack "FILE\n") t)            
      Nothing -> 
        return (TL.pack "NOTH\n")
  () | TL.unpack x == ("OK" ++ token)   -> liftIO $ do
    hPutStrLn lfh "-- READY. Waiting for event --------------------------" >> hFlush lfh
    hPutStrLn lfh "======================================================" >> hFlush lfh
    okLoop oldpathm oldsvg ch d lfh diff
  other -> liftIO $ do
    hPutStrLn lfh $ TL.unpack x
    putStrLn (TL.unpack x) >> hFlush lfh
    return (TL.pack "FILE ERROR")
