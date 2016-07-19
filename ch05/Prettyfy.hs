-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。
module Prettyfy where --これつけないとmainファイルだと思われてコンパイルしてくれない。

data Doc = Empty 
           | Char  Char
           |Text String
           |Line
           |Concat Doc Doc
           |Union Doc Doc

--doc値の区切り文字を入れる。  
punctuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds



--Stubs                     
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
