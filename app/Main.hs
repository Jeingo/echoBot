{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import System.IO()

main :: IO ()
main = do
  conf <- readConfig >>= makeMyConfig
  doRequest
  return ()

data ConfData = ConfData { helpText :: String
                         , repeatText :: String
                         , startRepeat :: String 
                         } deriving (Show)

data InitReq = InitTg { updateId :: String
                     , justId :: String
                     , message :: String
                     , username :: String
                     } deriving (Show)

data Env = Env { getInit :: InitReq
               , getConf :: ConfData 
               }

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

simpRequest :: String -> String
simpRequest tok = "https://api.telegram.org/bot" ++ tok ++ "/getUpdates" ++ "?timeout=15"

doRequest :: IO B.ByteString 
doRequest = do
  token <- readToken
  resp <- N.httpBS $ N.parseRequest_ $ simpRequest token 
  return $ N.getResponseBody resp 

makeMyInitResp :: B.ByteString -> IO InitReq
makeMyInitResp resp = do
  let upId = " "
  let jId = " "
  let mesg = " "
  let un = " "
  return $ InitTg upId jId mesg un

--jsonParser






