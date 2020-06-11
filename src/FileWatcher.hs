module FileWatcher (monitor) where

import Data.Time (UTCTime)
import Control.Concurrent (threadDelay)
import System.Directory (getModificationTime)

monitor :: Int -> String -> UTCTime -> IO () -> IO ()
monitor n file mtime handler = do
    threadDelay n -- sleep `n` ns
    t <- getModificationTime file
    if t > mtime
        then handler >> monitor n file t handler
        else monitor n file mtime handler