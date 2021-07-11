{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
module Game where

import Card
import Control.Lens
import Control.Monad.State
import Data.Tuple (swap)
import Control.Monad.Except
import Data.Functor
import Control.Arrow ((>>>))

type Deck = [Card]

type Foundation = [Card]

type FoundationIndex = Int

type Draws = [Card]

type Error = String

data Pile = Pile { _pileHiddens :: [Card]
                 -- ^ top card first
                 , _pileShowns :: [Card]
                 -- ^ top card first
                 } deriving(Eq, Ord)

type PileIndex = Int

-- | is the pile empty?
pileIsEmpty :: Pile -> Bool
pileIsEmpty (Pile [] []) = True
pileIsEmpty _ = False

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

-- | Optic for game pile
gamePile :: PileIndex -> Traversal' Game Pile
gamePile pileIndex = gamePiles . ix pileIndex

infixr `cons'`
cons' :: Applicative f => f a -> f [a] -> f [a]
cons' x xs = (:) <$> x <*> xs

-- | list-y lens for foundations for int-indexed gets
gameFoundations' :: Lens' Game [Foundation]
gameFoundations' = lens getter setter where
    getter g = let (a,b,c,d) = g ^. gameFoundations in [a,b,c,d]
    setter g [a, b, c, d] = g & gameFoundations .~ (a, b, c, d)
    setter _ _ = error "can only set with 4 foundation piles"

-- | optic for an individual foundation pile
gameFoundation :: FoundationIndex -> Traversal' Game Foundation
gameFoundation foundationIndex = gameFoundations' . ix foundationIndex

-- | a standard-order deck of cards
defaultDeck :: [Card]
defaultDeck = concat [withSuit Spades values, withSuit Diamonds values, withSuit Clubs (reverse values), withSuit Hearts (reverse values)] where
    withSuit s cs = flip Card s <$> cs
    values = [Ace .. King]

-- TODO shuffled

-- | the number of piles in a game of solitaire
numPiles :: Int
numPiles = 7

-- | get the top card from the deck and return it (does not put it in the draw pile)
popDeck :: (MonadState Game m, MonadError Error m) => m Card
popDeck = gets (^. gameDeck) >>= \case
    [] -> throwError "cannot draw from empty deck"
    c:cs -> (gameDeck .= cs) $> c

-- | get the top n cards from the deck and return them as a list
popDeckN :: (MonadState Game m, MonadError Error m) => Int -> m [Card]
popDeckN n = replicateM n popDeck

-- | A game with no cards at all, not even a deck
emptyGame :: Game
emptyGame = Game {_gameDeck = [], _gameDraws = [], _gameFoundations = ([], [], [], []), _gamePiles = []}

-- | construct a pile with n hidden cards and 1 shown card by drawing from the deck
makePile :: (MonadState Game m, MonadError Error m) => Int -> m Pile
makePile n = do
    hiddens <- popDeckN n
    shown <- popDeck
    return (Pile{_pileHiddens=hiddens, _pileShowns=[shown]})

-- | Create a game using the given deck
makeGame :: Deck -> Either Error Game
makeGame d = (do
    put (emptyGame{_gameDeck=d})
    piles <- mapM makePile [0..(pred numPiles)]
    gamePiles .= piles)
    & flip execStateT emptyGame
    & runExcept

-- | draw a card from the deck and put it on the draw pile
draw :: (MonadState Game m, MonadError Error m) => m ()
draw = do
    c <- popDeck
    gameDraws %= (c:)

-- | take the top card from the draw pile and return it (not the deck)
popDraw :: (MonadState Game m, MonadError Error m) => m Card
popDraw = gets (^. gameDraws) >>= \case
        [] -> throwError "expected a draw card"
        c:cs -> (gameDraws .= cs) $> c

-- | get pile at the given index. Must be in bounds
getPile :: (MonadState Game m, MonadError Error m) => PileIndex -> m Pile
getPile pileIndex = do
    mp <- gets (\g -> g ^? gamePile pileIndex)
    maybe (throwError ("pile index " ++ show pileIndex ++ " out of bounds")) return mp

-- | check the legality of putting a card on top of a pile. Must be 1 less than the top card and the opposite color, or any king if the pile is empty
checkPutCardOnPile :: (MonadError Error m) => Card -> Pile -> m ()
checkPutCardOnPile (Card King _) Pile{_pileShowns = []} = pure () -- king on empty ok
checkPutCardOnPile c Pile{_pileShowns = []} = throwError "can only put a king on an empty pile"
checkPutCardOnPile c Pile{_pileShowns = top:cs} = do
    unless (cardValue top - cardValue c == 1) (throwError "when putting a card on a pile, the card must be 1 less than the top card's value")
    unless (cardColor top /= cardColor c) (throwError "when putting a card on a pile, the card must have a different color than the top card")

-- | puts the given cards on the specified pile if legal. First card will be the top of the pile, last card will contact with the top of the pile
putCardsOnPile :: (MonadState Game m, MonadError Error m) => [Card] -> PileIndex -> m ()
putCardsOnPile [] _ = pure () -- putting no cards on is legal and does nothing, just need this match to call last in the other case
putCardsOnPile cs pileIndex = do
    let c = last cs
    pile <- getPile pileIndex
    checkPutCardOnPile c pile
    gamePiles . ix pileIndex . pileShowns %= (cs ++)

-- | puts the given card on top of the specified pile if legal
putCardOnPile :: (MonadState Game m, MonadError Error m) => Card -> PileIndex -> m ()
putCardOnPile c = putCardsOnPile [c]

-- | move the top draw card to the specified pile
drawToPile :: (MonadState Game m, MonadError Error m) => PileIndex -> m ()
drawToPile pileIndex = do
    c <- popDraw
    putCardOnPile c pileIndex

-- | get the foundation cards at the specified index. must be in bounds
getFoundation :: (MonadState Game m, MonadError Error m) => FoundationIndex -> m Foundation
getFoundation foundationIndex = do
    mf <- gets (\g -> g ^? gameFoundation foundationIndex)
    maybe (error ("foundation index "++show foundationIndex++" out of bounds")) return mf

-- | check the legality of putting a card on a foundation. Must be 1 value higher and same suit, or any ace if the pile is empty
checkPutCardOnFoundation :: (MonadError Error m) => Card -> Foundation -> m ()
checkPutCardOnFoundation c foundation = 
    case (foundation, c) of
        ([], Card Ace _) -> return () -- putting an ace on the empty
        ([], _) -> throwError "can only put an ace on an empty foundation pile"
        (top:_, _) -> do
            unless (cardValue c - cardValue top == 1) (throwError "when putting a card on a foundation pile, it must be 1 greater than the top card")
            unless (cardSuit c == cardSuit top) (throwError "when putting a card on a foundation pile, it must be the same suit as the top card")

-- | puts the card on the foundation if legal
putCardOnFoundation :: (MonadState Game m, MonadError Error m) => Card -> FoundationIndex -> m ()
putCardOnFoundation c foundationIndex = do
    foundation <- getFoundation foundationIndex
    checkPutCardOnFoundation c foundation
    gameFoundation foundationIndex %= (c:)

-- | move the draw card to the specified foundation
drawToFoundation :: (MonadState Game m, MonadError Error m) => FoundationIndex -> m ()
drawToFoundation foundationIndex = do
    c <- popDraw
    putCardOnFoundation c foundationIndex

-- | If there are hiddens, but no showns, flip the top hidden card
updatePileHiddens :: (MonadState Game m, MonadError Error m) => PileIndex -> m ()
updatePileHiddens pileIndex = getPile pileIndex >>= \case
    -- there are hiddens, but no showns. Turn over the top hidden
    Pile (h:hs) [] -> gamePiles . ix pileIndex .= Pile hs [h]
    _ -> pure ()

-- | take the top n cards from a pile and return them. Can only take shown cards. Shows the next card if all the showns were taken and there is a hidden
popNPile :: (MonadState Game m, MonadError Error m) => PileIndex -> Int -> m [Card]
popNPile pileIndex numCards = do
    pile <- getPile pileIndex
    let showns = pile ^. pileShowns
    unless (numCards <= length showns) (throwError ("tried to take "++show numCards++" cards from a pile, but only "++show (length showns)++" are available"))
    let (cards, showns') = splitAt numCards showns
    gamePiles . ix pileIndex . pileShowns .= showns'
    updatePileHiddens pileIndex
    pure cards

-- | take the top card from a pile and return it
popPile :: (MonadState Game m, MonadError Error m) => PileIndex -> m Card
popPile pileIndex = head <$> popNPile pileIndex 1

-- | move cards from one pile to another. numCards cards will be taken from sourcePile, starting from the top card
pileToPile :: (MonadState Game m, MonadError Error m) => PileIndex -> Int -> PileIndex -> m ()
pileToPile sourcePileIndex numCards targetPileIndex = do
    cards <- popNPile sourcePileIndex numCards
    putCardsOnPile cards targetPileIndex

-- | move the top card of a pile to a foundation
pileToFoundation :: (MonadState Game m, MonadError Error m) => PileIndex -> FoundationIndex -> m ()
pileToFoundation pileIndex foundationIndex = do
    c <- popPile pileIndex
    putCardOnFoundation c foundationIndex

-- | take the top card from the specified foundation and return it
popFoundation :: (MonadState Game m, MonadError Error m) => FoundationIndex -> m Card
popFoundation foundationIndex = do
    f <- getFoundation foundationIndex
    case f of
        [] -> throwError "cannot take card from an empty foundation"
        c:cs -> (gameFoundation foundationIndex .= cs) $> c

-- | move a card from a foundation to a pile
foundationToPile :: (MonadState Game m, MonadError Error m) => FoundationIndex -> PileIndex -> m ()
foundationToPile foundationIndex pileIndex = do
    c <- popFoundation foundationIndex
    putCardOnPile c pileIndex

data GameState = InProgress | Won | Lost

-- | get the current state of the game
getGameState :: (MonadState Game m) => m GameState
getGameState = do
    g <- get
    return $
        if null (g ^. gameDeck) && null (g ^. gameDraws) && all pileIsEmpty (g ^. gamePiles) -- all cards in foundations
            then Won
            else InProgress
    -- TODO loss analysis

runGame :: Game -> StateT Game (Except Error) a -> Either Error (a, Game)
runGame s = flip runStateT s >>> runExcept

runMove :: Game -> StateT Game (Except Error) a -> Either Error Game
runMove s = fmap snd . runGame s
