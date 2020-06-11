module Parsing ( toVector, produceFile, parseCSV, getHeader) where

import System.Environment
import Control.Monad (unless, forever)
import Data.Maybe
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Map as M
import System.IO (hIsEOF, openFile, Handle, IOMode(ReadMode), hGetLine, hClose)
import Data.Vector (Vector, snoc, empty, toList)

produceFileHandle :: Handle -> Producer String IO ()
produceFileHandle h = do
  eof <- lift $ hIsEOF h        -- 'lift' an 'IO' action from the base monad
  unless eof $ do
      str <- lift $ hGetLine h
      yield str            -- 'yield' the 'String'
      produceFileHandle h

produceFile :: String -> Producer String IO ()
produceFile filename = do
  h <- lift $ openFile filename ReadMode
  produceFileHandle h
  lift $ hClose h

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

parseCSV :: Pipe String (M.Map String Double) IO ()
parseCSV = do
  headerLn <- await
  let header = map (dropWhile (== ' ')) $ split ',' headerLn
  forever $ do
    nextLn <- await
    let
      numStrs = split ',' nextLn
      nums :: [Double]
      nums = map read numStrs
    yield $ M.fromList $ zip header nums

toVector :: Monad m => Producer a m () -> m (Vector a)
toVector = P.fold snoc empty id

getHeader :: Monad m => Producer (M.Map String Double) m () -> m [String]
getHeader = fmap (M.keys . fromMaybe M.empty) . P.head

someFunc :: IO ()
someFunc = do
  (filename:args) <- getArgs                  -- IO [String]
  putStrLn $ "Reading file: " ++ filename
  let
    prettyShow :: Show a => M.Map String a -> String
    prettyShow = concatMap (\(s, x) -> s ++ ": " ++ show x ++ ", ") . M.toList
  v <- toVector $ produceFile filename >-> parseCSV
  print v
  runEffect $ for (produceFile filename >-> parseCSV) (lift . putStrLn . prettyShow)
