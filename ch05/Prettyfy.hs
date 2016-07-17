-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。Prettyfyのモジュールを使ったAPIを使ったrenderJValueの新しい定義。これを満たすAPIを作る
import PrettyStub --TODO:スタブを本番にする
import SimpleJSON 
  
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
