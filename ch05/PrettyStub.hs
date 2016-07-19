--file: ch05/PrettyStub.hs
--Prettyfyモジュールのスタブを作ってPrettyfy.hsをテストする。
module PrettyStub where
import SimpleJSON
import Prettyfy
                   
--string:: String->Doc
--string str = undefined

text:: String->Doc
text str = undefined

double :: Double -> Doc
double num = undefined --undefinedはどこの値にもなれるが、評価しようとすると例外を発生させる。

fsep ::[Doc] ->Doc
fsep xs = undefined --Doc値のリストを合成し、出力が単一行に入りきらない場合行を折り返す。

--Doc値に対する連結演算子(:)
(<>):: Doc -> Doc -> Doc
a <> b = undefined
--一文字をDoc値に変換
char:: Char -> Doc
char r = undefined
--[Doc]を一つのDocに連結(concat)
hcat ::[Doc] -> Doc
hcat xs = undefined
