{-# LANGUAGE ScopedTypeVariables #-}

module QuadraticDelayRetrySpec where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Retry
import Control.Monad.Catch
import Data.Time.Clock
import Data.Time.LocalTime ()
import Data.IORef
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import Control.Monad.IO.Class
import System.IO.Error
import Control.Exception (IOException)

isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False


{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = return ()
 {-parallel $ describe "quadratic delay" $ do-}
  {-it "recovering test with quadratic retry delay"-}
     {-. property . monadicIO $ do-}
    {-startTime <- run $ getCurrentTime-}
    {-timeout <- (+2) . getSmall . getPositive <$> pick arbitrary-}
    {-retries <- pick . choose $ (0,10)-}
    {-[>run $ print $ show retries ++ ":" ++ show timeout<]-}
    {-res <- run . try $ recovering (RetrySettings (RLimit retries) True timeout)-}
                              {-[Handler (\(e::SomeException) -> return True)]-}
                              {-(throwM (userError "booo"))-}
    {-endTime <- run $ getCurrentTime-}
    {-QCM.assert (isLeftAnd isUserError res)-}
    {-let tmo = if retries > 0 then timeout * 2 ^ (retries - 1) else 0-}
    {-let ms' = ((fromInteger . toInteger $ tmo) / 1000.0)-}
    {-[>run $ print $ "ms':" ++ show ms'<]-}
    {-[>run $ print $ diffUTCTime endTime startTime<]-}
    {-QCM.assert (diffUTCTime endTime startTime >= ms')-}


failsAfter :: IORef Int -> Int -> Int -> Int -> IO b
failsAfter counter limit bef af = do
  c <- readIORef counter
  threadDelay (if c < limit then bef * 1000 else af * 1000)
  atomicModifyIORef' counter (\i -> (i+1,()))
  throwM (userError "booo")
