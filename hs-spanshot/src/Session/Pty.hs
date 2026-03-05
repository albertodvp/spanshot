{-# LANGUAGE CPP #-}

{- | PTY (pseudo-terminal) operations for Unix systems.

This module provides low-level PTY creation and I/O operations.
It uses the @unix@ package's 'openPseudoTerminal' for PTY management.

Unix-only: Windows builds include this module but functions return errors.
-}
module Session.Pty (
    -- * PTY Operations
    PtyHandle,
    spawnPty,
    readPty,
    writePty,
    closePty,
    resizePty,
) where

#if defined(mingw32_HOST_OS)

import Data.ByteString (ByteString)

-- | Opaque handle to a PTY (Windows stub)
data PtyHandle = PtyHandle

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

#else

import Data.ByteString (ByteString)

-- | Opaque handle to a PTY
data PtyHandle = PtyHandle
    { ptyMasterFd :: !Int
    , ptyChildPid :: !Int
    }

-- | Spawn a process in a PTY
spawnPty :: FilePath -> [String] -> IO (Either String PtyHandle)
spawnPty _ _ = error "Session.Pty.spawnPty: not yet implemented"

-- | Read from PTY
readPty :: PtyHandle -> IO (Either String ByteString)
readPty _ = error "Session.Pty.readPty: not yet implemented"

-- | Write to PTY
writePty :: PtyHandle -> ByteString -> IO (Either String ())
writePty _ _ = error "Session.Pty.writePty: not yet implemented"

-- | Close PTY
closePty :: PtyHandle -> IO ()
closePty _ = error "Session.Pty.closePty: not yet implemented"

-- | Resize PTY to new dimensions
resizePty :: PtyHandle -> Int -> Int -> IO (Either String ())
resizePty _ _ _ = error "Session.Pty.resizePty: not yet implemented"

#endif
