{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveGeneric #-}

module Foundation where

import Yesod
import Yesod.Core
import Yesod.Auth
import Yesod.Auth.OAuth2 (getAccessToken, getUserResponseJSON)
import Yesod.Auth.OAuth2.Google
import Yesod.Auth.OAuth2.Prelude (AccessToken)
import Yesod.WebSockets
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
import GHC.Generics

import WSConduit
import Watcher

data App = App {
  serverProto :: String,
  serverSite :: String,
  serverPort :: Int,
  serverURLPath :: String,
  clientId :: Text,
  clientSecret :: Text,
  httpManager :: Manager,
  logFileHandle :: Handle,
  csrf :: String,
  isPublic :: Bool,
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
    pack $ serverProto x ++ "://" ++ serverSite x ++ ":" ++ show (serverPort x) ++ serverURLPath x
--  approot = ApprootStatic "http://localhost:3000"

data GoogleUser
    = GoogleUser
    { name :: Text
    , email :: Text
    } deriving Generic
instance FromJSON GoogleUser

instance YesodAuth App where
  type AuthId App = Text
  getAuthId creds = case getUserResponseJSON creds of
    Right (GoogleUser nm eml) -> do
      setSession "_GMAIL" eml
      return $ Just eml
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins x = [ oauth2GoogleScoped ["email", "profile"] (clientId x) (clientSecret x) ]
  authHttpManager = getsYesod httpManager
  maybeAuthId = lookupSession "_GMAIL"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

giveSock :: App -> Handler Html
giveSock ysd = do
  let site = serverSite ysd
  let port = show $ serverPort ysd
  let path = serverURLPath ysd
  let ws   = case serverProto ysd of
        "http"  -> "ws" :: String
        "https" -> "wss" :: String
  let wsurl = ws ++ "://" ++ site ++ ":" ++ port ++ path ++ "/ws"
  let secureINIT = "INIT" ++ csrf ysd
  let secureOK = "OK" ++ csrf ysd
  let secureERROR = "ERROR" ++ csrf ysd
  oldsvg <- liftIO $ newTVarIO ""
  oldpathm <- liftIO $ newTVarIO Nothing
  c <- liftIO $ do
    nc <- newChan
    forkIO $ watchSVGFiles (dir ysd) nc (logFileHandle ysd)
    return nc
  webSockets (runConduit $ sourceWS .| serviceConduit oldpathm oldsvg c (csrf ysd) (dir ysd) (logFileHandle ysd) (diffProg ysd) .| sinkWSText)
  defaultLayout
    [whamlet|
            <button id="resend">Repair
            <span id="led_r">⬤
            <span id="led_g">⬤

            <div id="svg">  Nothing yet to show   
            
            <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/diff_match_patch/20121119/diff_match_patch.js">
            
            <script type="text/javascript">
              var prevText = "";
              var green = 0;
              var dmp = new diff_match_patch();
              var s = document.getElementById("svg");
              var conn = new WebSocket("#{wsurl}");
              const parser = new DOMParser();
              document.getElementById("resend").onclick = function() { conn.send("#{secureERROR}"); };
              conn.onopen = function(event) { conn.send("#{secureINIT}"); }
              conn.onmessage = function(message){
                var mdata = message.data ;
                if (mdata.substring(0,4) === "FILE") {
                  prevText = mdata.substring(5);
                  s.innerHTML = prevText;
                  console.log("Got FILE");
                  document.getElementById("led_r").style.color = "darkorange";
                  conn.send("#{secureOK}");
                } else if (mdata.substring(0,4) === "PTCH") {
                  console.log("Got PATCH");
                  var p = dmp.patch_fromText(mdata.substring(5));
                  var result = dmp.patch_apply(p, prevText);
                  var statuses = result.slice(1);
                  console.log(statuses.map(function(r) { return r.toString(); }).join("*"));
                  if (statuses.reduce(function(acc,x) { return acc && x ; },  true)) {
                    prevText = result[0];
                    const dom = parser.parseFromString(prevText, "application/xml");
                    if (dom.documentElement.nodeName == "parsererror") {
                      console.log("incomplete");
                      document.getElementById("led_r").style.color = "red";
                      conn.send("#{secureERROR}");
                    } else {
                      console.log(dom.documentElement.nodeName);
                      s.innerHTML = prevText;
                      conn.send("#{secureOK}");
                      green = (green + 125) % 256;
                      document.getElementById("led_g").style.color = `rgb(0,${green},0)`;
                      document.getElementById("led_r").style.color = "black";
                    }
                  } else {
                    console.log("ERROR patching");
                    document.getElementById("led_r").style.color = "red";
                    conn.send("#{secureERROR}");
                  }
                } else {
                    console.log(`Got ${mdata.substring(0,4)}`);
                    document.getElementById("led_r").style.color = "blue";
                    conn.send("#{secureOK}");
                }
              }
            |]
  
getHomeR :: Handler Html
getHomeR = do
  ysd <- getYesod
  maid <- maybeAuthId
  if isPublic ysd
    then giveSock ysd
    else
      case maid of
        Nothing -> defaultLayout [whamlet|
                                         <a href=@{AuthR LoginR} style="font-size:28pt"> Go to the login page
                                         |]
        Just aid | getAll (mconcat [ All (aid /= pack x) | x <- users ysd ]) ->
                   liftIO (
                     putStrLn "=== Unauthorized  user ===" >>
                     putStrLn (">>>" ++ show aid ++  "<<<") >>
                     putStrLn "==========================" >>
                     hPutStrLn (logFileHandle ysd) "=== Unauthorized  user ===" >>
                     hPutStrLn (logFileHandle ysd) (">>>" ++ show aid ++  "<<<") >>
                     hPutStrLn (logFileHandle ysd) "=========================="
                           ) >> defaultLayout [whamlet| User >>>#{show aid}<<< NOT AUTHORIZED |]
                 | otherwise -> giveSock ysd

        
