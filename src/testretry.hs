{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Concurrent
import Control.Retry
import Control.Monad.Catch
import Data.Time.Clock
import Data.Time.LocalTime ()
import Data.IORef

main :: IO b
main = do
  print "Start"
  x <- newIORef 0 :: IO (IORef Int)
  {-print "recovering"-}
  {-recovering (RetrySettings (RLimit 5) True 1000)-}
              {-[Handler (\(e::SomeException) -> return True)]-}
             {-(do-}

                {-t <- getCurrentTime-}
                {-print t-}
                {-throwM (userError "booo"))-}

  {-print "recoveringServer"-}
  {-recoveringServer (RetrySettings (RLimit 5) True 1000)-}
              {-[Handler (\(e::SomeException) -> return True)]-}
             {-(do-}
                {-t <- getCurrentTime-}
                {-print t-}
                {-throwM (userError "booo"))-}

  print "recoveringServer"
  recoveringServer (RetrySettings (RLimit 5) True 1000)
              [Handler (\(e::SomeException) -> return True)]
             (failsAfter6 x)

failsAfter6 counter = do
  c <- readIORef counter
  putStrLn $ "doing stuff:" ++ show c
  threadDelay (if c < 6 then 4000000 else 1000)
  print "not doing stuff"
  t <- getCurrentTime
  print t
  atomicModifyIORef' counter (\i -> (i+1,()))
  throwM (userError "booo")
