-- file: ch05/PrettyJSON.hs
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


