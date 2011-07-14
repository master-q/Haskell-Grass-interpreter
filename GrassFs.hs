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
  src :: B.ByteString
}

grassfsstate :: IORef GrassfsState
grassfsstate = unsafePerformIO $ newIORef GrassfsState { src = B.pack "wWWwwww" }

updateGrassfsState :: MonadIO m => (GrassfsState -> GrassfsState) -> m ()
updateGrassfsState fn = liftIO $! atomicModifyIORef grassfsstate $ \st -> (fn st, ())

queryGrassfsState :: MonadIO m => (GrassfsState -> a) -> m a
queryGrassfsState fn = liftM fn $ liftIO $! readIORef grassfsstate

main :: IO ()
main = fuseMain grassfsOps defaultExceptionHandler

grassfsOps :: FuseOperations HT
grassfsOps = defaultFuseOps { fuseGetFileStat = grassfsGetFileStat
                            , fuseOpen        = grassfsOpen
                            , fuseRead        = grassfsRead 
                            , fuseOpenDirectory = grassfsOpenDirectory
                            , fuseReadDirectory = grassfsReadDirectory
                            , fuseGetFileSystemStats = grassfsGetFileSystemStats
                            }
grassfsPath :: FilePath
grassfsPath = "/hello"

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

fileStat :: FuseContext -> B.ByteString -> FileStat
fileStat ctx source = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length source
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
    src' <- queryGrassfsState src
    return $ Right $ fileStat ctx src'
grassfsGetFileStat _ =
    return $ Left eNOENT

grassfsOpenDirectory :: FilePath -> IO Errno
grassfsOpenDirectory "/" = return eOK
grassfsOpenDirectory _   = return eNOENT

grassfsReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
grassfsReadDirectory "/" = do
    updateGrassfsState $ \s -> s { src = B.concat [src s, B.pack "1"] }
    src' <- queryGrassfsState src
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(grassfsName,    fileStat ctx src')
                   ]
    where (_:grassfsName) = grassfsPath
grassfsReadDirectory _ = return (Left eNOENT)

grassfsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
grassfsOpen path mode flags
    | path == grassfsPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


grassfsRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
grassfsRead path _ byteCount offset
    | path == grassfsPath = do
        grassfsSrc <- queryGrassfsState src
        let out = case toGrassCode $ B.unpack grassfsSrc of
              Left e -> "Error parsing input:" ++ show e
              Right r -> S.evalState stateGrass $ initGrassState r
        return $ Right $
          B.pack $ take (fromIntegral byteCount) $ 
          drop (fromIntegral offset) out
    | otherwise         = return $ Left eNOENT

grassfsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
grassfsGetFileSystemStats str =
  return $ Right FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
