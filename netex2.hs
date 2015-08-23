{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Prelude as P
import Happstack.Server
import Control.Applicative ((<$>), (<*>))
import Control.Exception as CE
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 as L
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Debug.Trace

newtype Temperatures = Temperatures [TempInfo] deriving (Eq, Show)

data TempInfo = TempInfo {
  dtime :: String,
  temp :: Integer
} deriving (Eq, Show)

isElementString :: String -> String -> Bool
isElementString [] _                        = True
isElementString _ []                        = False
isElementString s@(x:xs) (y:ys) | x == y    = isElementString xs ys
                                | otherwise = isElementString s ys

conf = nullConf { port = 5002 }
p = "JSON/weatherJSON.json";

main :: IO ()
main = do res <- CE.try $ readJSON' p
          case res of
            Left (ex :: IOException) -> simpleHTTP conf (ok $ "file not found: " ++ p)
            Right contents -> do
					let t = fromJust $ decodeJSONTemp contents
					let tInfo = toTemp t
					let tData = (findDate tInfo "") ++ (findTemp tInfo "657")
					simpleHTTP conf (ok tData)

findTemp :: [TempInfo] -> String -> String
findTemp (ti:[]) str  = ("Temperature not found: " ++ str ++ "\n")
findTemp (ti:tis) ""  = "Temperature not found\n"
findTemp (ti:tis) str = do
  let te = getTemp ti
  let next = P.init tis
  if (isElementString str te == True)
    then ("Temperature: " ++ te)
    else findTemp (next) str

findDate :: [TempInfo] -> String -> String
findDate (ti:[]) str  = ("Date not found: " ++ str ++ "\n")
findDate (ti:tis) ""  = "Date not found\n"
findDate (ti:tis) str = do
  let dt = getDateTime ti
  let next = P.init tis
  if (isElementString str dt == True)
    then ("Date: " ++ dt ++ "\n")
    else findDate (next) str
	
readJSON' :: String -> IO ByteString
readJSON' path = L.readFile path

instance FromJSON TempInfo where
    parseJSON (Object var) = TempInfo <$>
                             var .: "date" <*>
                             var .: "temperature"
	
instance ToJSON TempInfo where
    toJSON (TempInfo dtime temp) = object ["date" .= dtime, "temperature" .= temp]

toStr :: TempInfo -> String
toStr (TempInfo dt t) = (dt ++ ", " ++ show t)

encodeJSONObject :: TempInfo -> Value
encodeJSONObject (TempInfo dt t) = toJSON (TempInfo dt t)

decodeJSONObject :: ByteString -> Maybe [TempInfo]
decodeJSONObject bstr = (decode bstr :: Maybe [TempInfo])

getDateTime :: TempInfo -> String
getDateTime (TempInfo dt _) = dt

getTemp :: TempInfo -> String
getTemp (TempInfo _ t) = show t

toTemp :: Temperatures -> [TempInfo]
toTemp (Temperatures t) = t

instance FromJSON Temperatures where
    parseJSON (Object var) = Temperatures <$> (parseJSON =<< (var .: "temperatures"))

decodeJSONTemp :: ByteString -> Maybe Temperatures
decodeJSONTemp str = do
  t <- (decode str :: Maybe Temperatures)
  return t
