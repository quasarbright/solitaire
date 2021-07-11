{-# LANGUAGE FlexibleContexts #-}
module Move where

import Game
    ( PileIndex,
      Error,
      FoundationIndex,
      Game,
      draw,
      drawToPile,
      drawToFoundation,
      pileToPile,
      pileToFoundation,
      foundationToPile )
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)

data Move
    = Draw
    | DrawToFoundation FoundationIndex
    | DrawToPile PileIndex
    | FoundationToPile FoundationIndex PileIndex
    | PileToFoundation PileIndex FoundationIndex
    | PileToPile PileIndex Int PileIndex
    deriving(Eq, Ord, Show)

performMove :: (MonadState Game m, MonadError Error m) => Move -> m ()
performMove Draw = draw
performMove (DrawToFoundation fi) = drawToFoundation fi
performMove (DrawToPile pi) = drawToPile pi
performMove (FoundationToPile fi pi) = foundationToPile fi pi
performMove (PileToFoundation pi fi) = pileToFoundation pi fi
performMove (PileToPile pi1 n pi2) = pileToPile pi1 n pi2
