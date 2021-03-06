module Main (main) where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader

import System.Fuse

import qualified Control.Monad.State as S
import Grass

type HT = ()

data GrassfsState = GrassfsState {
  src :: String
}

-- global value
grassfsstate :: IORef GrassfsState
grassfsstate = unsafePerformIO $ newIORef GrassfsState { src = "wWWwwww" }

updateGrassfsState :: MonadIO m => (GrassfsState -> GrassfsState) -> m ()
updateGrassfsState fn = liftIO $! atomicModifyIORef grassfsstate $ \st -> (fn st, ())

queryGrassfsState :: MonadIO m => (GrassfsState -> a) -> m a
queryGrassfsState fn = liftM fn $ liftIO $! readIORef grassfsstate

updateSrc :: MonadIO m => (String -> String) -> m ()
updateSrc fn = updateGrassfsState (\s -> s { src = fn $ src s })

injectString :: Int -> String -> String -> String
injectString offset istr tstr = map fn pstr
  where fn (Nothing, ts) = ts
        fn (Just is, _) = is
        pstr = zip (replicate offset Nothing ++ map Just istr)
               (take m (tstr ++ repeat ' '))
          where m = max (offset + length istr) (length tstr)

main :: IO ()
main = fuseMain grassfsOps defaultExceptionHandler

grassfsOps :: FuseOperations HT
grassfsOps = defaultFuseOps { fuseGetFileStat = grassfsGetFileStat
                            , fuseOpen        = grassfsOpen
                            , fuseSetFileSize = grassfsSetFileSize
                            , fuseRead        = grassfsRead 
                            , fuseWrite       = grassfsWrite
                            , fuseOpenDirectory = grassfsOpenDirectory
                            , fuseReadDirectory = grassfsReadDirectory
                            , fuseGetFileSystemStats = grassfsGetFileSystemStats
                            }
grassfsPath :: FilePath
grassfsPath = "/grassvm"

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

mylength :: [a] -> Int
mylength l =  len l 0
  where
    len :: [a] -> Int -> Int
    len [] n = n
    len (_:xs) n | n > (1024 * 1024) = n -- 無限に続きそうなら諦める
                 | otherwise = len xs (n + 1)

fileStat :: FuseContext -> String -> FileStat
fileStat ctx s = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , ownerWriteMode
                                           , groupReadMode
                                           , groupWriteMode
                                           , otherReadMode
                                           , otherWriteMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ mylength s
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

grassfsGetFileStat :: FilePath -> IO (Either Errno FileStat)
grassfsGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
grassfsGetFileStat path | path == grassfsPath = do
    ctx <- getFuseContext
    grassfsSrc <- queryGrassfsState src
    return $ Right $ fileStat ctx (runGrassSrc grassfsSrc)
grassfsGetFileStat _ =
    return $ Left eNOENT

grassfsOpenDirectory :: FilePath -> IO Errno
grassfsOpenDirectory "/" = return eOK
grassfsOpenDirectory _   = return eNOENT

grassfsSetFileSize :: FilePath -> FileOffset -> IO Errno
grassfsSetFileSize path offset | path == grassfsPath = do
  updateSrc (\s -> take (fromIntegral offset) (s ++ repeat ' '))
  return eOK
grassfsSetFileSize _ _ = return eNOENT

grassfsReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
grassfsReadDirectory "/" = do
    ctx <- getFuseContext
    grassfsSrc <- queryGrassfsState src
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(grassfsName,    fileStat ctx (runGrassSrc grassfsSrc))
                   ]
    where (_:grassfsName) = grassfsPath
grassfsReadDirectory _ = return (Left eNOENT)

grassfsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
grassfsOpen path _ _
    | path == grassfsPath = return (Right ())
    | otherwise         = return (Left eNOENT)

runGrassSrc :: String -> String
runGrassSrc s = case toGrassCode s of
  Left e -> "Error parsing input:" ++ show e ++ "\nsrc:\n" ++ s
  Right r -> S.evalState stateGrass $ initGrassState r

grassfsRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
grassfsRead path _ byteCount offset
    | path == grassfsPath = do
        grassfsSrc <- queryGrassfsState src
        let outchop = take (fromIntegral byteCount) $
                      drop (fromIntegral offset) (runGrassSrc grassfsSrc)
        if null outchop
          then return (Left eINVAL)
          else return $ Right $ B.pack outchop
    | otherwise         = return $ Left eNOENT

grassfsWrite :: FilePath -> HT -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
grassfsWrite path _ bstr offset
  | path == grassfsPath = do
    updateSrc $ injectString (fromIntegral offset) $ B.unpack bstr
    return $ Right $ fromIntegral $ B.length bstr
  | otherwise = return $ Left eNOENT

grassfsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
grassfsGetFileSystemStats _ =
  return $ Right FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
