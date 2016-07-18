-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。Prettyfyのモジュールを使ったAPIを使ったrenderJValueの新しい定義。これを満たすAPIを作る
module Prettyfy where --これつけないとmainファイルだと思われてコンパイルしてくれない。

import SimpleJSON 
import PrettyStub --TODO:スタブを本番にする
  
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray a) = series '[' ']' renderJValue ary
renderJValue (JObject o) = series '{' '}' field o
  where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val

        
puncuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
