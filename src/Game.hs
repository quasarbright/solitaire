{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Game where

import Card
import Control.Lens
import Control.Monad.State
import Data.Tuple (swap)

type Deck = [Card]

type Foundation = [Card]

type Draws = [Card]

data Pile = Pile { _pileHiddens :: [Card]
                 -- ^ top card first
                 , _pileShowns :: [Card]
                 -- ^ top card first
                 } deriving(Eq, Ord)

makeLenses ''Pile


data Game = Game { _gameDeck :: Deck
                 -- ^ invisible pile you draw from
                 , _gameDraws :: Draws
                 -- ^ visible draw cards (only top is visible)
                 , _gameFoundations :: (Foundation, Foundation, Foundation, Foundation)
                 -- ^ foundations (where you build starting with aces)
                 , _gamePiles :: [Pile]
                 -- ^ piles (where you build starting with kings)
                 }
                 deriving(Eq, Ord)

makeLenses ''Game

defaultDeck :: [Card]
defaultDeck = [Card n s | n <- [1..13], s <- [Hearts, Diamonds, Clubs, Spades]]

numPiles :: Int
numPiles = 7

popDraw :: MonadState Game m => m (Maybe Card)
popDraw = state go where
    go :: Game -> (Maybe Card, Game)
    go g = case _gameDeck g of
        [] -> (Nothing, g)
        c:cs -> (Just c, g{_gameDeck = cs})

popDrawUnsafe :: MonadState Game m => m Card
popDrawUnsafe = do
    mc <- popDraw
    case mc of
        Nothing -> error "expected a draw card"
        Just c -> return c

popNDrawUnsafe :: MonadState Game m => Int -> m [Card]
popNDrawUnsafe n = replicateM n popDrawUnsafe

emptyGame :: Game
emptyGame = Game {_gameDeck = [], _gameDraws = [], _gameFoundations = ([], [], [], []), _gamePiles = []}

makePile :: MonadState Game m => Int -> m Pile
makePile n = do
    hiddens <- popNDrawUnsafe n
    shown <- popDrawUnsafe
    return (Pile{_pileHiddens=hiddens, _pileShowns=[shown]})

makeGame :: Deck -> Game
makeGame d = flip execState emptyGame $ do
    put (emptyGame{_gameDeck=d})
    piles <- mapM makePile [0..(succ numPiles)]
    gamePiles .= piles


