-- file: ch08/ElfMagic.hs
import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic c = L.take 4 content ==elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

