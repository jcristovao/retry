{-# LANGUAGE ScopedTypeVariables #-}

module RetrySpec where

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
spec = parallel $ describe "retry" $ do

  {-it "recovering test without quadratic retry delay"-}
     {-. property . monadicIO $ do-}
    {-startTime <- run $ getCurrentTime-}
    {-timeout <- (+2) . getSmall . getPositive <$> pick arbitrary-}
    {-retries <- getSmall . getPositive <$> pick arbitrary-}
    {-res <- run . try $ recovering (RetrySettings (RLimit retries) False timeout)-}
                              {-[Handler (\(e::SomeException) -> return True)]-}
                              {-(throwM (userError "booo"))-}
    {-endTime <- run $ getCurrentTime-}
    {-QCM.assert (isLeftAnd isUserError res)-}
    {-let ms' = ((fromInteger . toInteger $ (timeout * retries)) / 1000.0)-}
    {-QCM.assert (diffUTCTime endTime startTime >= ms')-}


  it "IO actions starts failing after _limit_ times."
     . property . monadicIO $ do
    x <- run $ newIORef 0 :: PropertyM IO (IORef Int)
    limit   <- getPositive <$> pick arbitrary
    timeout <- (+2) . getPositive <$> pick arbitrary
    retries <- getPositive <$> pick arbitrary
    res <- run . try $
          (recoveringWatchdog (RetrySettings (RLimit retries) False timeout)
                              timeout
                              [Handler (\(e::SomeException) -> return True)]
                              (failsAfter x limit timeout 1)
          ) :: PropertyM IO (Either IOException a)
    inspect <- run $ readIORef x
    run $ print $ isLeftAnd isUserError res
    run $ print $ show inspect ++ "==" ++ show limit ++ " + " ++ show retries
    QCM.assert (isLeftAnd isUserError res)
    QCM.assert (inspect == (limit + retries))


  {-print "recoveringServer"-}
  {-recoveringServer (RetrySettings (RLimit 5) True 1000)-}
              {-[Handler (\(e::SomeException) -> return True)]-}
             {-(do-}
                {-t <- getCurrentTime-}
                {-print t-}
                {-throwM (userError "booo"))-}

failsAfter :: IORef Int -> Int -> Int -> Int -> IO b
failsAfter counter limit bef af = do
  c <- readIORef counter
  threadDelay (if c < limit then bef * 1000 else af * 1000)
  atomicModifyIORef' counter (\i -> (i+1,()))
  throwM (userError "booo")
