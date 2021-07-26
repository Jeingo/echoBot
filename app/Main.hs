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
                         , startRepeat :: Int 
                         } deriving (Show)

data InitReq = InitTg { updateId :: Int 
                      , justId :: Int 
                      , message :: T.Text 
                      } deriving (Show)



--configurator
readConfig :: IO CT.Config 
readConfig = C.load [C.Required "src/cfg/confBot.cfg"]

makeMyConfig :: CT.Config -> IO ConfData
makeMyConfig conf = do
  hT <- C.require conf (T.pack "main.helpText") :: IO String
  rT <- C.require conf (T.pack "main.repeatText") :: IO String
  sR <- C.require conf (T.pack "main.startRepeat") :: IO Int 
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
helper Nothing = InitTg 1 1 " "

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
    return $ InitTg upId jId mesg 

  parseJSON _ = mzero


--main Func
mainFunc :: ConfData -> InitReq ->  IO ()
mainFunc conf fstInit = do
  --print $ message fstInit
  case ( message fstInit ) of
    "/help" -> print $ helpText conf
    "/repeat" -> print $ repeatText conf
    _ -> print "Word"
  tok <- readToken
  let req = sendMessage tok fstInit
  _ <- N.httpNoBody $ N.parseRequest_ $ req 
  nextStep fstInit
  return ()

nextStep :: InitReq -> IO ()
nextStep fstIn = do
  tok <- readToken
  let offset = show $ (updateId fstIn) + 1
  let req = "https://api.telegram.org/bot" ++ tok ++ "/getUpdates" ++ "?offset=" ++ offset 
  _ <- N.httpNoBody $ N.parseRequest_ $ req 
  return ()


sendMessage :: String -> InitReq -> String
sendMessage tok initR = "https://api.telegram.org/bot" ++ tok ++ "/sendMessage" ++ "?chat_id=" ++ chatId ++ "&text=" ++ textMess 
  where chatId = show $ updateId initR
        textMess = T.unpack $ message initR






