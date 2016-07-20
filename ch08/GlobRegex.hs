-- file: ch08/GlobRegex.hs
module GlobRegex
       (
         globToRegex
       , matchesGlob
         )where

import Text.Regex.Posix ((=~))
--glob正規表現をposix正規表現に変換するルール。文字列の先頭から始まり文字列の末尾で終了するために、位置が固定されていなければならない。
globToRegex :: String -> String 
globToRegex cs = '^' : globToRegex' cs ++"$"

globToRegex' = undefined

matchesGlob =undefined