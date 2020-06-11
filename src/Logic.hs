{-# LANGUAGE TemplateHaskell #-}

module Logic (handleEvent, CSVViewState(..), Tick, Name, contents, header, vrange, filename, hrange) where

import Data.Vector
import Data.Map
import Control.Lens
import Brick
import qualified Graphics.Vty as V

data CSVViewState = CSVViewState {
    _filename :: String,
    _contents :: Vector (Map String Double), -- The actual CSV data
    _header :: [String], -- The header
    _hrange :: (Int, Int), -- The vertical view range (start row and length).
    _vrange :: (Int, Int) -- The horizontal view range (start col and length).
  }
makeLenses ''CSVViewState

data ScrollDirection = ScrollUp | ScrollDown | ScrollLeft | ScrollRight

scroll :: ScrollDirection -> CSVViewState -> CSVViewState
scroll ScrollUp = over (vrange._1) (+ (- 1))
scroll ScrollDown = over (vrange._1) (+ 1)
scroll ScrollLeft = over (hrange._1) (+ (- 1))
scroll ScrollRight = over (hrange._1) (+ 1)

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

handleEvent :: CSVViewState -> BrickEvent Name Tick -> EventM Name (Next CSVViewState)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ scroll ScrollUp g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ scroll ScrollDown g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))         = continue $ scroll ScrollLeft g
handleEvent g (VtyEvent (V.EvKey V.KRight []))       = continue $ scroll ScrollRight g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g