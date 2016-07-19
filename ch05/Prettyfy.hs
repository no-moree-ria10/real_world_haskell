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

--[Doc]を一つのDocに連結(concat)
hcat ::[Doc] -> Doc
hcat = fold (<>)

--Emptyに対する畳み込み(上で使う）
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

--Doc値のリストを合成し、出力が単一行に入りきらない場合行を折り返す
fsep ::[Doc] ->Doc
fsep  = fold (</>)

(</>) :: Doc -> Doc -> Doc  --折り返し関数
x </> y = x <> softline <> y

softline :: Doc  --柔らかい改行
softline = group line

--union構成子を用いることでドキュメントの選択肢（改行入れる/入れない)を２つ持つ。出力時に実際に開業するかのロジックを入れる
group :: Doc -> Doc           
group x = flatten x `Union` x 
--左側の要素が常に右より同じ幅もしくは広い幅になる。
flatten :: Doc -> Doc 
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _ ) = flatten x 
flatten other = other 

compact :: [Doc] -> String                
compact x = transform x
  where transform [] = ""
        transform (d:ds) = 
          case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            a `Concat` b -> transform(a:b:ds)
            _ `Union` b -> transform(b:ds)

        
--doc値の区切り文字を入れる。  
punctuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds



