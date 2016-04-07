module System.ReadEnvVar
    ( readEnvVar
    , readEnvVarDef
    ) where

import Data.Maybe (fromMaybe)
import System.Environment (getEnv, lookupEnv)
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
readEnvVar :: Read a => String -> IO (Maybe a)
readEnvVar envVar = do
    maybeEnvVal <- lookupEnv envVar
    case maybeEnvVal of
        Nothing -> return Nothing
        Just envVal -> return $ readMaybe envVal
