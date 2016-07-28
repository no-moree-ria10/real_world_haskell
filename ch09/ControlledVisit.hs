-- file: ch09/ControlledVisit.hs
import Control.Monad 
import System.Directory 
import Data.Time ( UTCTime(..) )
import System.FilePath
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
--以前書いた関数
import RecursiveContents (getRecursiveContents)
--Exception Tipe Signature用
import GHC.Exception


data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer  
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

--与えれたディレクトリの情報             
getInfo :: FilePath -> IO Info             
getInfo = undefined
traverse :: ( [Info] -> [Info] ) -> FilePath -> IO [Info] 
traverse order path = do 
  names <- getUseFulContents path
  contents <- mapM getInfo ( path : map ( path </> ) names )
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUseFulContents :: FilePath -> IO [String]  
getUseFulContents path = do 
  names<- getDirectoryContents path 
  return ( filter (`notElem` [".", ".."] ) names)

isDirectory :: Info -> Bool  
isDirectory = maybe False searchable . infoPerms