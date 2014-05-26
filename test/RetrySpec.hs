{-# LANGUAGE ScopedTypeVariables #-}

module RetrySpec where

import Control.Applicative
import Control.Retry
import Control.Monad.Catch
import Data.Time.Clock
import Data.Time.LocalTime ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import System.IO.Error
import Data.Metrology
import Data.Metrology.SI

isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False


{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = parallel $ describe "retry" $ do

  it "recovering test without quadratic retry delay"
     . property . monadicIO $ do
    startTime <- run getCurrentTime
    timeout <- fmap (% milli Second) . pick . choose $ (0,15)
    retries <- getSmall . getPositive <$> pick arbitrary
    res <- run . try $ recovering (RetrySettings (RLimit retries) False timeout)
                              [Handler (\(_::SomeException) -> return True)]
                              (throwM (userError "booo"))
    endTime <- run getCurrentTime
    QCM.assert (isLeftAnd isUserError res)
    let ms' = ((fromInteger . toInteger $ ((timeout #! milli Second) * retries)) / 1000.0)
    QCM.assert (diffUTCTime endTime startTime >= ms')
