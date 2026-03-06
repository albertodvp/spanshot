{-# LANGUAGE CPP #-}

{- | PTY (pseudo-terminal) operations for Unix systems.

This module provides low-level PTY creation and I/O operations.
It uses the @unix@ package's 'openPseudoTerminal' for PTY management.

Unix-only: Windows builds include this module but functions return errors.
-}
module Session.Pty (
    -- * PTY Operations
    PtyHandle (..),
    spawnPty,
    readPty,
    writePty,
    closePty,
    resizePty,
    waitPty,
) where

#if defined(mingw32_HOST_OS)

import Data.ByteString (ByteString)
import System.Exit (ExitCode)
import System.Posix.Types (Fd)
import System.Posix.Types (ProcessID)

-- | Opaque handle to a PTY (Windows stub)
data PtyHandle = PtyHandle
    { ptyMasterFd :: !Fd
    , ptyChildPid :: !ProcessID
    }
    deriving (Show)

-- | Spawn a process in a PTY (Windows stub)
spawnPty :: FilePath -> [String] -> IO (Either String PtyHandle)
spawnPty _ _ = pure $ Left "PTY not supported on Windows"

-- | Read from PTY (Windows stub)
readPty :: PtyHandle -> IO (Either String ByteString)
readPty _ = pure $ Left "PTY not supported on Windows"

-- | Write to PTY (Windows stub)
writePty :: PtyHandle -> ByteString -> IO (Either String ())
writePty _ _ = pure $ Left "PTY not supported on Windows"

-- | Close PTY (Windows stub)
closePty :: PtyHandle -> IO ()
closePty _ = pure ()

-- | Resize PTY (Windows stub)
resizePty :: PtyHandle -> Int -> Int -> IO (Either String ())
resizePty _ _ _ = pure $ Left "PTY not supported on Windows"

-- | Wait for PTY child process (Windows stub)
waitPty :: PtyHandle -> IO ExitCode
waitPty _ = pure $ ExitFailure 1

#else

import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Error (Errno (..), getErrno)
import System.Exit (ExitCode (..))
import System.Posix.IO (closeFd, dupTo, fdReadBuf, fdWriteBuf, stdInput, stdOutput, stdError)
import System.Posix.Process (ProcessStatus (..), forkProcess, executeFile, getProcessStatus)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd, ProcessID)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)

-- | Handle to a PTY with master FD and child process ID
data PtyHandle = PtyHandle
    { ptyMasterFd :: !Fd
    , ptyChildPid :: !ProcessID
    }
    deriving (Show)

{- | Spawn a process in a PTY.

Creates a pseudo-terminal, forks, and executes the given command
in the child process with the slave PTY as its controlling terminal.

Returns a handle that can be used to read/write to the process.
-}
spawnPty :: FilePath -> [String] -> IO (Either String PtyHandle)
spawnPty cmd args = do
    result <- try $ do
        -- Open a new pseudo-terminal pair
        (masterFd, slaveFd) <- openPseudoTerminal

        -- Fork a child process
        pid <- forkProcess $ do
            -- Child process: set up the slave PTY as controlling terminal
            -- Close the master side in child
            closeFd masterFd

            -- Redirect stdin/stdout/stderr to the slave PTY
            _ <- dupTo slaveFd stdInput
            _ <- dupTo slaveFd stdOutput
            _ <- dupTo slaveFd stdError

            -- Close the original slave FD (it's now duplicated)
            closeFd slaveFd

            -- Execute the command
            executeFile cmd True args Nothing

        -- Parent process: close the slave side
        closeFd slaveFd

        pure PtyHandle
            { ptyMasterFd = masterFd
            , ptyChildPid = pid
            }

    case result of
        Left (e :: IOError) -> pure $ Left $ "Failed to spawn PTY: " <> show e
        Right handle -> pure $ Right handle

{- | Read data from the PTY master.

Reads up to 4096 bytes from the PTY. Returns empty ByteString on EOF.
-}
readPty :: PtyHandle -> IO (Either String ByteString)
readPty handle = do
    result <- try $ allocaBytes bufSize $ \buf -> do
        bytesRead <- fdReadBuf (ptyMasterFd handle) buf (fromIntegral bufSize)
        if bytesRead <= 0
            then pure BS.empty
            else BS.packCStringLen (castPtr buf, fromIntegral bytesRead)
    case result of
        Left (e :: IOError) -> do
            errno <- getErrno
            -- EIO (errno 5) is expected when the child process exits
            if errno == Errno 5
                then pure $ Right BS.empty
                else pure $ Left $ "Failed to read from PTY: " <> show e
        Right bs -> pure $ Right bs
  where
    bufSize = 4096

{- | Write data to the PTY master.

Writes the given bytes to the PTY, which will be received by the child
process as if typed on the terminal.
-}
writePty :: PtyHandle -> ByteString -> IO (Either String ())
writePty handle bs
    | BS.null bs = pure $ Right ()
    | otherwise = do
        result <- try $ BS.useAsCStringLen bs $ \(ptr, len) -> do
            _ <- fdWriteBuf (ptyMasterFd handle) (castPtr ptr) (fromIntegral len)
            pure ()
        case result of
            Left (e :: IOError) -> pure $ Left $ "Failed to write to PTY: " <> show e
            Right () -> pure $ Right ()

-- | Close the PTY and terminate the child process if still running.
closePty :: PtyHandle -> IO ()
closePty handle = do
    -- Try to kill the child process if it's still running
    _ <- try @IOError $ signalProcess sigKILL (ptyChildPid handle)
    -- Close the master FD
    _ <- try @IOError $ closeFd (ptyMasterFd handle)
    pure ()

{- | Resize the PTY to new dimensions.

Currently a no-op stub - full implementation requires TIOCSWINSZ ioctl.
-}
resizePty :: PtyHandle -> Int -> Int -> IO (Either String ())
resizePty _handle _rows _cols = do
    -- TODO: Implement using TIOCSWINSZ ioctl
    -- For now, this is a no-op
    pure $ Right ()

{- | Wait for the PTY child process to exit.

Returns the exit code of the child process.
-}
waitPty :: PtyHandle -> IO ExitCode
waitPty handle = do
    mStatus <- getProcessStatus True False (ptyChildPid handle)
    case mStatus of
        Nothing -> pure $ ExitFailure 1
        Just (Exited code) -> pure code
        Just (Terminated _ _) -> pure $ ExitFailure 1
        Just (Stopped _) -> pure $ ExitFailure 1

#endif
