{-# LANGUAGE CPP #-}

{- | Wrap mode for single command monitoring.

This module provides the ability to run a single command with
SpanShot monitoring, preserving the exit code while capturing
any errors with context.
-}
module Wrap (
    -- * Wrap Operations
    runWrap,
    WrapResult (..),
) where

import System.Exit (ExitCode)

#if defined(mingw32_HOST_OS)

import System.Exit (ExitFailure, exitWith)
import System.IO (hPutStrLn, stderr)

-- | Result of a wrapped command execution
data WrapResult = WrapResult
    { wrapExitCode :: !ExitCode
    , wrapCaptureCount :: !Int
    }
    deriving (Show, Eq)

-- | Run a command with SpanShot monitoring (Windows stub)
runWrap :: FilePath -> [String] -> IO WrapResult
runWrap _ _ = do
    hPutStrLn stderr "Error: wrap mode is not supported on Windows"
    exitWith (ExitFailure 1)

#else

-- | Result of a wrapped command execution
data WrapResult = WrapResult
    { wrapExitCode :: !ExitCode
    , wrapCaptureCount :: !Int
    }
    deriving (Show, Eq)

-- | Run a command with SpanShot monitoring (Unix implementation)
runWrap :: FilePath -> [String] -> IO WrapResult
runWrap _ _ = error "Wrap.runWrap: not yet implemented"

#endif
