module UI (drawUI) where

import Prelude hiding (length)

import Logic
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Data.Map as M
import Data.Vector ((!), length, slice, toList)
import Control.Lens

drawUI :: CSVViewState -> [Widget Name]
drawUI s = [drawCSV s]


drawCSV :: CSVViewState -> Widget Name
drawCSV s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ s ^. filename)
  $ hBox cols
  where
    start = min (max (s^.vrange._1) 0) (length (s ^. contents) - 1 )
    numRows = max 0 (min (s^.vrange._2) (length (s ^. contents) - start - 1 ))
    resData = map (M.map (: [])) $ toList $ slice start numRows (s^.contents)
    mergedData = M.unionsWith (++) resData
    cols = tail $ concat [[B.vBorder, vBox $ cellsInCol c] | c <- drop (s^.hrange._1) (s^.header)]
    cellsInCol c = str c : [str $ show x | x <- M.findWithDefault [] c mergedData]
