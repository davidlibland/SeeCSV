module Main where

import System.Environment
import Brick
import Parsing
import Logic
import UI
import Pipes ((>->))
import qualified Graphics.Vty as V
import Control.Monad (void)

app :: App CSVViewState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
--main = someFunc
main = do
  (filename:args) <- getArgs                  -- IO [String]
  putStrLn $ "Reading file: " ++ filename
  let csvData = produceFile filename >-> parseCSV
  v <- toVector csvData
  h <- getHeader csvData
  let
    initialState = CSVViewState {
      _filename = filename,
      _contents = v,
      _header = h,
      _vrange = (0, 50),
      _hrange = (0, 10)
    }
  void $ defaultMain app initialState
