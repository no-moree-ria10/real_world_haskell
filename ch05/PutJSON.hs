-- file: ch05/PutJSON.hs
module PutJSON where

import Data.List (intercalate)
import SimpleJSON

--renderJValue :jvalueを文字列に変換  
renderJValue :: JValue -> String

renderJValue(JString s) = show s
renderJValue(JNumber n) = show n 
renderJValue(JValue True) = "true"
renderJValue(JValue False) = "false"
renderJValue JNull = "null"

renderJValue(JObject o) = "{" ++ pairs o ++ "}"
                          where pairs[] = ""
                                pairs ps = intercalate ", " (map renderPair ps)
                                renderPair(k,v) = show k ++ ": " ++ renderJValue v

renderJValue(JArray a) = "[" ++ values a ++ "]"
                         where values[] = ""
                               values vs = intercalate ", " (map renderJValue vs)
--end
                               
-- putJValue :上の文字列を出力 文字列を生成する関数とそれを出力する関数は分けておいた方がいい。出力形式がいろいろあるときに応用が効く
putJValue :: JValue -> IO()
putJValue v = putStrLn $ renderJValue v
-- end 
