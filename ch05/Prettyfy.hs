-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。Prettyfyのモジュールを使ったAPIを使ったrenderJValueの新しい定義。これを満たすAPIを作る
module Prettyfy where --これつけないとmainファイルだと思われてコンパイルしてくれない。

import SimpleJSON 
import PrettyStub --TODO:スタブを本番にする
        
punctuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
