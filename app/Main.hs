
import Yesod.Core
import Options.Applicative
import Data.Semigroup ((<>))
import Network.HTTP.Client.Conduit (Manager, newManager)
import Data.Text
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import System.Entropy
import System.IO
import Sound.Pulse.Simple

import Config
import Foundation
import Watcher

data Clops = Clops { xmlCommon :: String , xmlInstance :: String }

cloparser :: Parser Clops
cloparser = Clops
  <$> (strOption ( short 'c' <> metavar "COMM" <> help "XML common config" ))
  <*> (strOption ( short 'i' <> metavar "INST" <> help "XML instance config"))


main :: IO ()
main = do
  clops <- execParser $
    info
    (cloparser <**> helper)
    (fullDesc <>
     progDesc "Server SVG" <>
     header "Serve SVG")
  cfgComm <- getCommonConfig $ xmlCommon clops
  cfgInst <- getInstanceConfig $ xmlInstance clops
  log <- openFile (Config.logFile cfgInst) WriteMode
  bpr <- simpleNew Nothing "SVGServer2" Play Nothing "beeper on SVGServer2"
    (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  rnd <- getEntropy 64
  let token = C8.unpack $ B64.encode rnd
  man <- newManager
  let tls = tlsSettings (cert cfgComm) (key cfgComm)
  a <- toWaiApp (App {
                    serverProto = proto cfgComm,
                    serverSite = site cfgComm,
                    serverPort = remotePort cfgInst,
                    clientId = pack $ googleId cfgComm,
                    clientSecret = pack $ googleSecret cfgComm,
                    httpManager = man,
                    logFileHandle = log,
                    csrf = token,
                    Foundation.users = Config.users cfgInst,
                    Foundation.dir = Config.dir cfgInst,
                    Foundation.diffProg = Config.diffProg cfgComm,
                    beeper = bpr
                    })
  runTLS tls (setPort (localPort cfgInst) defaultSettings) a
  simpleFree bpr
  hClose log
