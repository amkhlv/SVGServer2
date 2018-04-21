{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes           #-}

module Foundation where

import Yesod
import Yesod.Core
import Yesod.Auth
import Yesod.Auth.GoogleEmail2
import Yesod.WebSockets
import Data.Text
import Network.HTTP.Conduit (Manager, newManager)
import Yesod.Form.Fields
import qualified Data.Text.Lazy as TL
import Data.Text
import Data.Conduit
import qualified Data.Conduit.List
import Control.Concurrent.Chan
import System.FSNotify
import Control.Concurrent.STM.TVar
import Data.Monoid
import Control.Concurrent (forkIO)
import System.IO

import WSConduit
import Watcher

data App = App {
  serverProto :: String,
  serverSite :: String,
  serverPort :: Int,
  clientId :: Text,
  clientSecret :: Text,
  httpManager :: Manager,
  logFileHandle :: Handle,
  csrf :: String,
  users :: [String],
  dir :: String,
  diffProg :: String
  }

mkYesod "App" [parseRoutes| 
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
  approot = ApprootMaster $ \x ->
    pack $ serverProto x ++ "://" ++ serverSite x ++ ":" ++ (show $ serverPort x)
--  approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins x = [ authGoogleEmail (clientId x) (clientSecret x) ]
  authHttpManager = getsYesod httpManager
  maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
  ysd <- getYesod
  maid <- maybeAuthId
  case maid of
    Nothing -> defaultLayout [whamlet|
                                     <a href=@{AuthR LoginR}> Go to the login page
                                     |]
    Just aid | getAll (mconcat [ All (not $ show aid == x) | x <- users ysd ]) ->
               ( liftIO $
                 putStrLn "=== Unauthorized  user ===" >>
                 putStrLn (">>>" ++ show aid ++  "<<<") >>
                 putStrLn "==========================" >>
                 hPutStrLn (logFileHandle ysd) "=== Unauthorized  user ===" >>
                 hPutStrLn (logFileHandle ysd) (">>>" ++ show aid ++  "<<<") >>
                 hPutStrLn (logFileHandle ysd) "=========================="
               ) >>
               defaultLayout [whamlet| Not authorized |]
             | otherwise -> do
      let site = serverSite ysd
      let port = show $ serverPort ysd
      let ws   = case serverProto ysd of
            "http"  -> "ws" :: String
            "https" -> "wss" :: String
      let wsurl = ws ++ "://" ++ site ++ ":" ++ port
      let secureINIT = "INIT" ++ (csrf ysd)
      let secureOK = "OK" ++ (csrf ysd)
      oldsvg <- liftIO $ newTVarIO ""
      oldpathm <- liftIO $ newTVarIO Nothing
      c <- liftIO $ do
        nc <- newChan
        forkIO $ watchSVGFiles (dir ysd) nc (logFileHandle ysd)
        return nc
      webSockets (runConduit $ sourceWS .| serviceConduit oldpathm oldsvg c (csrf ysd) (dir ysd) (logFileHandle ysd) (diffProg ysd) .| sinkWSText)
      defaultLayout
        [whamlet|
                <div id="svg">  Nothing yet to show   
                
                <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/diff_match_patch/20121119/diff_match_patch.js">
                
                <script type="text/javascript">
                  var prevText = "";
                  var dmp = new diff_match_patch();
                  var s = document.getElementById("svg");
                  var conn = new WebSocket("#{wsurl}");
                  conn.onopen = function(event) { conn.send("#{secureINIT}"); }
                  conn.onmessage = function(message){
                    var mdata = message.data ;
                    if (mdata.substring(0,4) === "FILE") {
                      prevText = mdata.substring(5);
                    } else if (mdata.substring(0,4) === "PTCH") {
                      var p = dmp.patch_fromText(mdata.substring(5));
                      var result = dmp.patch_apply(p, prevText);
                      prevText = result[0];
                    }
                    s.innerHTML = prevText;
                    conn.send("#{secureOK}");
                  }
                |]

        
