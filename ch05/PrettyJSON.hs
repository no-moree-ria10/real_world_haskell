-- file: ch05/PrettyJSON.hs
--Prettyfyのモジュールを使ったAPIを使ったrenderJValueの新しい定義を先に作った。これを満たすAPIを作る
module PrettyJSON(
  renderJValue
  )where
--import
import PrettyStub
import SimpleJSON(JValue(..))
import Prettyfy
--data type Doc
--data Doc = ToBeDefined deriving(Show)
--renderJvalue
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' field o
  where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val

