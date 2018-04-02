{-# LANGUAGE Arrows #-}
module Config where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import Data.Maybe(fromJust)
import Data.Map


data CommonCfg = CommonCfg
  { proto :: String
  , site  :: String
  , googleId :: String
  , googleSecret :: String
  , diffProg     :: String
  , cert :: String
  , key  :: String
  } deriving (Show, Eq)

data InstanceCfg = InstanceCfg
  { port :: Int
  , dir :: String
  , users :: [String]
  , logFile :: String
  } deriving (Show, Eq)

instance XmlPickler CommonCfg where xpickle = xpCommonCfg
xpCommonCfg :: PU CommonCfg
xpCommonCfg =
  xpElem "config" $
  xpWrap (\((h,u,i,s,d,c,k)) -> CommonCfg h u i s d c k,
           \cf ->
             (proto cf, site cf, googleId cf, googleSecret cf, diffProg cf, cert cf, key cf)) $
  xp7Tuple
  (xpElem "proto" xpText)
  (xpElem "site" xpText)
  (xpElem "GoogleClientID" xpText)
  (xpElem "GoogleClientSecret" xpText)
  (xpElem "diffprog" xpText)
  (xpElem "cert" xpText)
  (xpElem "key" xpText)

instance XmlPickler InstanceCfg where xpickle = xpInstanceCfg
xpInstanceCfg :: PU InstanceCfg
xpInstanceCfg =
  xpElem "config" $
  xpWrap (\((p, d, u, l)) -> InstanceCfg p d u l ,
           \cf ->
             (port cf, dir cf, users cf, logFile cf)) $
  xp4Tuple
  (xpElem "port" xpInt)
  (xpElem "dir" xpText)
  (xpElem "users" $ xpList $ xpElem  "user" xpText)
  (xpElem "log" xpText)

getCommonConfig :: String -> IO CommonCfg
getCommonConfig x = (fromJust . head) <$> (runX $
                                           readDocument [withRemoveWS yes] x >>>
                                           getChildren >>>
                                           isElem >>^
                                           unpickleDoc xpCommonCfg)

getInstanceConfig :: String -> IO InstanceCfg
getInstanceConfig x = (fromJust . head) <$> (runX $
                                             readDocument [withRemoveWS yes] x >>>
                                             getChildren >>>
                                             isElem >>^
                                             unpickleDoc xpInstanceCfg)
