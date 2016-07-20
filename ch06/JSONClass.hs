-- file: ch06/JSONClass.hs
{-# LANGUAGE TypeSynonymInstances #-} --型パラメータを具体化したデータ型を型クラスのinstanceにするのを許す言語拡張
{-# LANGUAGE FlexibleInstances #-}

module JSONClass 
       (
         JAry(..)
       )where

import SimpleJSON

data JValue = JString String 
            | JNumber Double 
            | JBool Bool 
            | JNull 
            | JObject (JObj JValue) --was JObject [ ( String, JValue ) ] 
            | JArray (JAry JValue) --was JArray [JValue]            
            deriving (Eq, Ord, Show)
  
type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a
  
instance JSON JValue where
  toJValue = id
  fromJValue = Right
  
--値コンストラクタの種類に関わらない統一的なインターフェースを定義
instance JSON Bool where  
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "not a JSON boolean"
  
instance JSON String where  
  toJValue = JString
  fromJValue(JString s) = Right s
  fromJValue _ = Left "not a JSON string"

--JNumber vから整数と実数への共通インターフェース  
doubleToJValue :: (Double ->a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "Not a JSON number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id
  
newtype JAry a = JAry{
  fromJAry :: [a] 
  } deriving(Eq, Ord, Show)

newtype JObj a = JObj{            
  fromJObj :: [(String,a)]
  } deriving(Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryToJValue ::(JSON a) => JAry a -> JValue                 
instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue
  
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJary :: [JValue] -> JAry JValue 
jvaluesToJary = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue                
jaryOfJValuesToJValue = JArray

jaryToJValue =  jaryOfJValuesToJValue . jvaluesToJary . listToJValues . fromJAry

