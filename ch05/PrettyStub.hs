--file: ch05/PrettyStub.hs
--Prettyfyモジュールのスタブを作ってPrettyfy.hsをテストする。
module PrettyStub where
import SimpleJSON

data Doc = ToBeDefined
           deriving(Show)
                   
string:: String->Doc
string str = undefined

text:: String->Doc
text str = undefined

double :: Double -> Doc
double num = undefined --undefinedはどこの値にもなれるが、評価しようとすると例外を発生させる。