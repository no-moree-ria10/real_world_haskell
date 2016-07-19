-- file: ch-5/Prettyfy.hs
-- Prettyモジュールはtext関数とdouble関数とstring関数を提供する。
module Prettyfy where --これつけないとmainファイルだと思われてコンパイルしてくれない。

import PrettyStub --TODO:スタブを本番にする
import Numeric(showHex)
import Data.Bits(shiftR, (.&.))
import Data.Char(ord)


  
--[start]String -> Doc変換関数
--Doc文字列はクォートされた文字の列
string:: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

--Doc値を開き文字と閉じ文字でくくる         
enclose:: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
--ここの文字列をエスケープするか表示可能形式に変換
oneChar:: Char -> Doc
oneChar c = case lookup c simpleEscapes of Just r -> text r
                                           Nothing | mustEscape c->hexEscape c
                                                   | otherwise -> char c
                                           where mustEscape c = c < ' ' || c =='\x7f' || c > '\xff' -- if c == ASCII  -> true
--改行文字などを出力するためにchar(\n) -> String("/n")という文字列に変換するための連想配列                                               
simpleEscapes :: [(Char, String)] 
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b] )
--ASCII文字以外をエスケープするための関数 
hexEscape ::Char ->Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
  where d = ord c
--smallHex 文字コードをunicode文字に変換し、docに入れる。でも0xffffまでしか表現できない！！（0x10ffffまでほしい) 0xffff以上を表現するには、2つにビットを分割する必要がある
smallHex::Int -> Doc
smallHex x = text "\\u"
             <> text ( replicate ( 4 - length h ) '0' )
             <> text h
  where h = showHex x "" --showHex: 数の16進数表記を返す
        
--6桁の以上の文字コードをぶんかつしてエンコーディング
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = ( n `shiftR` 10) .&. 0x3ff -- 10 = 0x001100
        b = n .&. 0x3ff
        
--[end]

--[start]配列とオブジェクトのプリティプリンタ
--配列オブジェクト共通のプリンタ構造（ { or [ , 中身 , ] or })
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate ( char ',') . map item

--doc値の区切り文字を入れる。  
punctuate:: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
