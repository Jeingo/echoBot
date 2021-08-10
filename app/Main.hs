{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Aeson
import System.IO()
import Control.Monad (mzero)

--mainFunc
main :: IO ()
main = do
  conf <- readConfig >>= makeMyConfig
  mainFunc conf $ Map.empty
  return ()

--data
data ConfData = ConfData { helpText :: String
                         , repeatText :: String
                         , startRepeat :: Int 
                         , button :: String
                         } deriving (Show)

data InitReq = InitTg { updateId :: Int 
                      , justId :: Int 
                      , message :: T.Text 
                      } deriving (Show, Eq)

data InitReqButton = InitTgB { updateIdB :: Int
                             , justIdB :: Int
                             , countB :: T.Text
                             } deriving (Show, Eq)



--configurator
readConfig :: IO CT.Config 
readConfig = C.load [C.Required "src/cfg/confBot.cfg"]

makeMyConfig :: CT.Config -> IO ConfData
makeMyConfig conf = do
  hT <- C.require conf (T.pack "main.helpText") :: IO String
  rT <- C.require conf (T.pack "main.repeatText") :: IO String
  sR <- C.require conf (T.pack "main.startRepeat") :: IO Int 
  bt <- C.require conf (T.pack "main.button") :: IO String
  return $ ConfData hT rT sR bt
  
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

makeMyInitRespB :: B.ByteString -> IO InitReqButton
makeMyInitRespB resp = do
  let myInit = decodeStrict resp :: Maybe InitReqButton
  let res = helperB myInit
  return res

helper :: Maybe InitReq -> InitReq
helper (Just a) = a
helper Nothing = InitTg 1 1 " "

helperB :: Maybe InitReqButton -> InitReqButton
helperB (Just a) = a
helperB Nothing = InitTgB 1 1 " " 

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

instance FromJSON InitReqButton where
  parseJSON (Object req) = do
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    callback <- arr .: "callback_query"
    count <- callback .: "data"
    from <- callback .: "from"
    jId <- from .: "id"
    return $ InitTgB upId jId count

  parseJSON _ = mzero


--main Func
mainFunc :: ConfData -> Map.Map Int Int -> IO ()
mainFunc conf counter = do
  fstInitTmp <- doRequest
  fstInit <- makeMyInitResp fstInitTmp
  fstInitB <- makeMyInitRespB fstInitTmp

  let newCounter = if Map.member (justId fstInit) counter
                      then counter 
                      else Map.insert (justId fstInit) (startRepeat conf) counter
  if (fstInit /= (InitTg 1 1 " "))
    then do
          case ( message fstInit ) of
            "/help" -> sendHelpText (helpText conf) fstInit
            "/repeat" -> testKeyboard (B8.fromString $ button conf) fstInit
            _ -> sendMesToTg (Map.lookup (justId fstInit) newCounter) fstInit 
    else do
          print $ countB fstInitB        
          nextStepB fstInitB

  nextStep fstInit

  return ()

nextStep :: InitReq -> IO ()
nextStep fstIn = do
  tok <- readToken
  let offset = show $ (updateId fstIn) + 1
  let req = "https://api.telegram.org/bot" ++ tok ++ "/getUpdates" ++ "?offset=" ++ offset 
  N.httpNoBody $ N.parseRequest_ $ req 
  return ()

nextStepB :: InitReqButton -> IO ()
nextStepB fstIn = do
  tok <- readToken
  let offset = show $ (updateIdB fstIn) + 1
  let req = "https://api.telegram.org/bot" ++ tok ++ "/getUpdates" ++ "?offset=" ++ offset 
  N.httpNoBody $ N.parseRequest_ $ req 
  return ()

  
sendHelpText :: String -> InitReq -> IO ()
sendHelpText helpT fstInit = do
  tok <- readToken
  let chatId = show $ justId fstInit
  let req = "https://api.telegram.org/bot" ++ tok ++ "/sendMessage" ++ "?chat_id=" ++ chatId ++ "&text=" ++ helpT 
  N.httpNoBody $ N.parseRequest_ $ req
  return ()


sendMessage :: String -> InitReq -> String
sendMessage tok initR = "https://api.telegram.org/bot" ++ tok ++ "/sendMessage" ++ "?chat_id=" ++ chatId ++ "&text=" ++ textMess 
  where chatId = show $ justId initR
        textMess = T.unpack $ message initR

sendMesToTg :: Maybe Int -> InitReq -> IO ()
sendMesToTg count fstInit = do
  let counter = case count of
                    Just a -> a
                    Nothing -> 1
  let words = replicate counter (message fstInit) 
  --mapM_ (\s -> print s) words
  tok <- readToken
  let req = sendMessage tok fstInit
  mapM_ (\s -> N.httpNoBody $ N.parseRequest_ $ req) words


testKeyboard :: B.ByteString -> InitReq -> IO () 
testKeyboard keyB fstInit = do
  tok <- readToken 
  let chatId = B8.fromString $ show $  justId fstInit
  request' <- N.parseRequest $ "POST https://api.telegram.org/bot" ++ tok ++ "/sendMessage"
  let req = N.setRequestQueryString [("chat_id", Just $ chatId),
                                     ("text", Just "Choose how many repeating:"), 
                                     ("reply_markup", Just keyB)] $ request'
  N.httpNoBody req
  return () 




  -- Нужно будет добавить в основную функцию обработку чисел с клавиатуры
  -- Для этого нужно будет дополнить парсер Aeson в InitReq
  -- То есть фокус на аесон а затем основную 
  -- Создать новый тип данных - и затем в main добавить ветвление (или кнопка или слово )

