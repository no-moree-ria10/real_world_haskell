-- file: ch05/PrettyJSON.hs
module PrettyJSON where
--import
import Numeric(showHex)

--data type Doc
data Doc = ToBeDefined deriving(Show)
--Doc文字列はクォートされた文字の列
string:: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

--Doc値を開き文字と閉じ文字でくくる         
enclose:: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
--Doc値に対する連結演算子(:)
(<>):: Doc -> Doc -> Doc
a <> b = undefined
--一文字をDoc値に変換
char:: Char -> Doc
char r = undefined
--[Doc]を一つのDocに連結(concat)
hcat ::[Doc] -> Doc
hcat xs = undefined
--ここの文字列をエスケープするか表示可能形式に変換
oneChar:: Char -> Doc
oneChar c = case lookup c simpleEscapes of Just r -> text r
                                           Nothing | mustEscape c->hexEscape c
                                                   | otherwise -> char c
                                           where mustEscape c = c < ' ' || c =='\x7f' || c > '\xff' -- if c == ASCII  -> true
--改行文字などを出力するためにchar(\n) -> String("/n")という文字列に変換するための連想配列                                               
simpleEscapes :: [(Char, String)] 
simpleEscapes zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b] )
--ASCII文字以外をエスケープするための関数 
hexEscape  

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
        b - n .&. 0x3ff










