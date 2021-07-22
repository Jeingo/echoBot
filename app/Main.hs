{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Aeson
import System.IO()
import Control.Monad (mzero)

main :: IO ()
main = do
  conf <- readConfig >>= makeMyConfig
  myIni <- doRequest >>= makeMyInitResp
  mainFunc conf myIni
  return ()

--data
data ConfData = ConfData { helpText :: String
                         , repeatText :: String
                         , startRepeat :: String 
                         } deriving (Show)

data InitReq = InitTg { updateId :: Int 
                      , justId :: Int 
                      , message :: T.Text 
                      , username :: T.Text 
                      } deriving (Show)

data Env = Env { getInit :: InitReq
               , getConf :: ConfData 
               }

--configurator
readConfig :: IO CT.Config 
readConfig = C.load [C.Required "src/cfg/confBot.cfg"]

makeMyConfig :: CT.Config -> IO ConfData
makeMyConfig conf = do
  hT <- C.require conf (T.pack "main.helpText") :: IO String
  rT <- C.require conf (T.pack "main.repeatText") :: IO String
  sR <- C.require conf (T.pack "main.startRepeat") :: IO String
  return $ ConfData hT rT sR
  
readToken :: IO String 
readToken = do
  con <- readFile "src/cfg/token"
  return $ filter (/= '\n') con

--first request
simpRequest :: String -> String
simpRequest tok = "https://api.telegram.org/bot" ++ tok ++ "/getUpdates" ++ "?timeout=15"

doRequest :: IO B.ByteString 
doRequest = do
  token <- readToken
  resp <- N.httpBS $ N.parseRequest_ $ simpRequest token 
  return $ N.getResponseBody resp 

makeMyInitResp :: B.ByteString -> IO InitReq
makeMyInitResp resp = do
  let myInit = decodeStrict resp :: Maybe InitReq
  let res = helper myInit 
  return res 

helper :: Maybe InitReq -> InitReq
helper (Just a) = a
helper Nothing = InitTg 1 1 " " " "

--jsonParser
instance FromJSON InitReq where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    mesg <- mes .: "text" 
    from <- mes .: "from"
    jId <- from .: "id"
    un <- from .: "username"
    return $ InitTg upId jId mesg un

  parseJSON _ = mzero

--main Func
mainFunc :: ConfData -> InitReq -> IO ()
mainFunc conf fstInit = do
  return ()
