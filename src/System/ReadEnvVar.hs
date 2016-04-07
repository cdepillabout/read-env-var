{-# LANGUAGE CPP #-}

{-|
Module      : System.ReadEnvVar
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This Haskell module exports functions for safely reading environment variables.
-}

module System.ReadEnvVar
    ( readEnvVar
    , readEnvVarDef
    ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Lookup a value from an environment variable and read it in with
-- 'readMaybe'.  If the environment variable doesn't exist, or it can't be
-- 'read', use the default value.
--
-- >>> import System.Environment (setEnv)
-- >>> setEnv "TEST_ENV_VAR" "1000"
-- >>> readEnvVarDef "TEST_ENV_VAR" 5 :: IO Int
-- 1000
-- >>> readEnvVarDef "THIS_ENV_VAR_WILL_NOT_EXIST" 5 :: IO Int
-- 5
readEnvVarDef :: Read a
              => String -- ^ environment variable to lookup
              -> a      -- ^ default value to use if the environment variable
                        -- either does not exist, or cannot be 'read'
              -> IO a
readEnvVarDef envVar def = fromMaybe def <$> readEnvVar envVar

-- | Lookup a value from an environment variable and read it in with
-- 'readMaybe'.
--
-- >>> import System.Environment (setEnv)
-- >>> setEnv "TEST_ENV_VAR" "2000"
-- >>> readEnvVar "TEST_ENV_VAR" :: IO (Maybe Int)
-- Just 2000
-- >>> readEnvVar "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO (Maybe Int)
-- Nothing
readEnvVar :: Read a
           => String       -- ^ environment variable to lookup
           -> IO (Maybe a)
readEnvVar = fmap (>>= readMaybe) . lookupEnv
