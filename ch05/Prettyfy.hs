-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。
module Prettyfy where --これつけないとmainファイルだと思われてコンパイルしてくれない。

data Doc = Empty 
           | Char  Char
           |Text String
           |Line  -- imply \n 
           |Concat Doc Doc --tree
           |Union Doc Doc -- tree

--constructer 
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text t = Text t

double :: Double -> Doc
double d = text ( show d )             

line :: Doc           
line = Line

--Doc値に対する連結演算子(:)
(<>):: Doc -> Doc -> Doc
--(Text t) <> (Text t') = text ( t ++ t')
--(Char c) <> (Text t) = text (c : t)
--(Text t) <> (Char c) = text (
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y
       
--doc値の区切り文字を入れる。  
punctuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds



--Stubs                     

fsep ::[Doc] ->Doc
fsep xs = undefined --Doc値のリストを合成し、出力が単一行に入りきらない場合行を折り返す。

--[Doc]を一つのDocに連結(concat)
hcat ::[Doc] -> Doc
hcat xs = undefined
