import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)


data ParseState = ParseState
                  {
                    string :: L.ByteString
                  , offset :: Int64
                  } deriving (Show) --アクセサを使うことで拡張性大
                             
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined
-- 失敗時の付加情報              
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }


identity :: a -> Parse a
identity a = Parse ( \s -> Right(a, s) )

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState 
  = case runParse parser ( ParseState initState 0 ) of
    Left err -> Left err
    Right (result, _ ) -> Right result
                          
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = 
  initState { offset = newOffset }

