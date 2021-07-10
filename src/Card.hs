{-# LANGUAGE PatternSynonyms #-}
module Card where

data Suit = Hearts | Diamonds | Spades | Clubs deriving(Eq, Ord)

data SuitColor = Red | Black deriving(Eq, Ord)

suitColor :: Suit -> SuitColor
suitColor Hearts = Red
suitColor Diamonds = Red
suitColor Spades = Black
suitColor Clubs = Black

instance Show Suit where
    show Hearts = "♥"
    show Diamonds = "♦"
    show Spades = "♠"
    show Clubs = "♣"

data Card = Card{ cardValue :: Int, cardSuit :: Suit } deriving(Eq, Ord)

cardColor :: Card -> SuitColor
cardColor = suitColor . cardSuit

pattern Ace :: Int
pattern Ace = 1
pattern Jack :: Int
pattern Jack = 11
pattern Queen :: Int
pattern Queen = 12
pattern King :: Int
pattern King = 13

showValue :: Int -> [Char]
showValue Ace = "A"
showValue Jack = "J"
showValue Queen = "Q"
showValue King = "K"
showValue n = show n

instance Show Card where
    show (Card n s) = showValue n++" "++show s
