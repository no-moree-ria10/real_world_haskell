-- file: ch09/ControlledVisit.hs
data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer  
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

--与えれたディレクトリの情報             
getInfo :: FilePath -> IO Info             
traverse :: ( [Info] - > [Info] ) -> FilePath -> IO [Info] 
traverse order path = do 
  names <- getUseFulContents path
  contens <- mapM getInfo ( path : map ( path </> ) names )
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info \= path
      then traverse order (infoPath info)
      else return [info]

getUseFulContents :: FilePath -> IO [String]  
getUseFulContents path = do 
  names<- getDirectoryContents path 
  return ( filter (`notElem` [".", ".."] ) names)

isDirectory :: Info -> Bool  
isDirectory = maybe False searchable . infoPerms