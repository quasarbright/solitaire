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

showValue :: Int -> [Char]
showValue 1 = "A"
showValue 11 = "J"
showValue 12 = "Q"
showValue 13 = "K"
showValue n = show n

instance Show Card where
    show (Card n s) = showValue n++" "++show s

-- | card must be 1 less and opposite color
canPutOnPileTop :: Card -> Card -> Bool
canPutOnPileTop card top = diff == 1 && cardColor top /= cardColor card where diff = cardValue top - cardValue card

-- | card must be 1 more and same suit
canPutOnFoundationTop :: Card -> Card -> Bool
canPutOnFoundationTop card top = diff == 1 && cardSuit top == cardSuit card where diff = cardValue card - cardValue top


