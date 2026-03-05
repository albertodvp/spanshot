{-# LANGUAGE CPP #-}

{- | PTY session management for interactive terminal monitoring.

This module provides the main entry point for PTY-based session mode,
where a developer can work in a monitored shell and have errors
captured automatically.

Unix-only: Windows returns "not supported" error.
-}
module Session (
    -- * Session Operations
    runSession,
) where

#if defined(mingw32_HOST_OS)

import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

-- | Run a PTY session (Windows stub - not supported)
runSession :: IO ()
runSession = do
    hPutStrLn stderr "Error: session mode is not supported on Windows"
    exitWith (ExitFailure 1)

#else

-- | Run a PTY session (Unix implementation)
runSession :: IO ()
runSession = error "Session.runSession: not yet implemented"

#endif
