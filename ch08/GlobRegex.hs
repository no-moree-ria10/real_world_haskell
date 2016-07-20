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

globToRegex' :: String -> String                 
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) ="[^" ++ c : charClass cs
globToRegex' ('[':c:cs) ='[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         |otherwise = [c]                                 
  where regexChars = "\\+()^$.{}]|"
        
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c: charClass cs
charClass [] = error "unterminated character class"
                      
                         
matchesGlob =undefined