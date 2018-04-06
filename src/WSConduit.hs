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
import           System.IO
import           Sound.Pulse.Simple

serviceConduit :: MonadIO m  =>
  TVar (Maybe FilePath) -> TVar T.Text -> Chan Event -> String -> String -> Handle -> String -> Simple ->
  ConduitT TL.Text TL.Text m ()
serviceConduit oldpathm oldsvg ch token d lfh diff beeper = mapMC $ \x -> case () of
  () | TL.unpack x == ("INIT" ++ token) -> liftIO $ do
         simpleWrite beeper ([sin $ 2*pi*293*(t/44100)|t<-[1..4410*5]] :: [Float]) >> simpleDrain beeper
         return (TL.pack "NOTH\n")
  () | TL.unpack x == ("OK" ++ token)   -> liftIO $ do
    simpleWrite beeper ([sin $ 2*pi*392*(t/44100)|t<-[1..4410*2]] :: [Float]) >> simpleDrain beeper
    hPutStrLn lfh "-- waiting for event" >> hFlush lfh
    ev <- readChan ch
    hPutStrLn lfh (show ev) >> hFlush lfh
    simpleWrite beeper ([sin $ 2*pi*440*(t/44100)|t<-[1..4410*2]] :: [Float]) >> simpleDrain beeper
    threadDelay 300000
    let fp = case ev of
          Added fp _ -> Just fp
          Modified fp _ -> Just fp
          Removed fp _  -> Nothing 
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
            oldContents <- atomically $ do
              o <- readTVar oldsvg
              writeTVar oldsvg (TL.toStrict t)
              return o
            hPutStr hin (T.unpack oldContents)
            hClose hin
            e <- hGetContents herr
            putStrLn e
            hPutStrLn lfh e >> hFlush lfh
            p <- hGetContents hout 
            (length p) `seq` hPutStrLn lfh ("PATCH size = " ++ (show $ length p))
            hClose herr
            hClose hout
            waitForProcess phand
            return (TL.pack $ "PTCH\n" ++ p)
          _ -> do
            t <- TLIO.readFile filepath
            atomically $ writeTVar oldsvg (TL.toStrict t)
            return (TL.append (TL.pack "FILE\n") t)            
      Nothing -> return (TL.pack "NOTH\n")
  other -> liftIO $ do
    hPutStrLn lfh $ TL.unpack x
    putStrLn (TL.unpack x) >> hFlush lfh
    return (TL.pack "FILE ERROR")
