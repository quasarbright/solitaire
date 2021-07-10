{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Game where

import Card
import Control.Lens
import Control.Monad.State
import Data.Tuple (swap)
import Control.Monad.Except
import Data.Functor

type Deck = [Card]

type Foundation = [Card]

type Draws = [Card]

type Error = String

data Pile = Pile { _pileHiddens :: [Card]
                 -- ^ top card first
                 , _pileShowns :: [Card]
                 -- ^ top card first
                 } deriving(Eq, Ord)

pattern EmptyPile = Pile [] []

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

-- TODO shuffled

numPiles :: Int
numPiles = 7

-- | get the top card from the deck and return it (does not put it in the draw pile)
popDeck :: (MonadState Game m, MonadError Error m) => m Card
popDeck = gets (^. gameDeck) >>= \case
    [] -> throwError "cannot draw from empty deck"
    c:cs -> (gameDeck .= cs) $> c

popDeckN :: (MonadState Game m, MonadError Error m) => Int -> m [Card]
popDeckN n = replicateM n popDeck

emptyGame :: Game
emptyGame = Game {_gameDeck = [], _gameDraws = [], _gameFoundations = ([], [], [], []), _gamePiles = []}

makePile :: (MonadState Game m, MonadError Error m) => Int -> m Pile
makePile n = do
    hiddens <- popDeckN n
    shown <- popDeck
    return (Pile{_pileHiddens=hiddens, _pileShowns=[shown]})

-- | Create a game from the given deck
makeGame :: Deck -> Either Error Game
makeGame d = (do
    put (emptyGame{_gameDeck=d})
    piles <- mapM makePile [0..(succ numPiles)]
    gamePiles .= piles)
    & flip execStateT emptyGame
    & runExcept

-- | draw a card from the deck and put it on the draw pile
draw :: (MonadState Game m, MonadError Error m) => m ()
draw = do
    c <- popDeck
    gameDraws %= (c:)

popDraw :: (MonadState Game m, MonadError Error m) => m Card
popDraw = gets (^. gameDraws) >>= \case
        [] -> throwError "expected a draw card"
        c:cs -> (gameDraws .= cs) $> c

getPile :: (MonadState Game m, MonadError Error m) => Int -> m Pile
getPile pileIndex = do
    mp <- gets (\g -> g ^. gamePiles ^? ix pileIndex)
    maybe (throwError ("pile index " ++ show pileIndex ++ " out of bounds")) return mp

checkPutCardOnPile :: (MonadError Error m) => Card -> Pile -> m ()
checkPutCardOnPile (Card 13 _) Pile{_pileShowns = []} = pure () -- king on empty ok
checkPutCardOnPile c Pile{_pileShowns = []} = throwError ("can only put a king on an empty pile, tried to put " ++ show c)
checkPutCardOnPile c Pile{_pileShowns = top:cs} = do
    unless (cardValue top - cardValue c == 1) (throwError "when putting a card on a pile, the card must be 1 less than the top card's value")
    unless (cardColor top /= cardColor c) (throwError "when putting a card on a pile, the card must have a different color than the top card")

putCardsOnPile :: (MonadState Game m, MonadError Error m) => [Card] -> Int -> m ()
putCardsOnPile [] _ = pure ()

-- | move the top draw card to the specified pile
-- drawToPile :: (MonadState Game m, MonadError Error m) => Int -> m ()
-- drawToPile pileIndex = do
--     c <- popDraw
    

