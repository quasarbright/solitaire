module Render(renderGame) where

import Game
import Card
import Text.PrettyPrint.Boxes
import Data.Foldable
import Control.Lens

{-
Deck: ??
Draw: VS
Foundations: VS VS VS VS
Piles:
VS ?? ??
   VS ??
      VS

1  2  3


hidden cards are ??
blank spots are --

-}

bCard :: Card -> Box
bCard = text . show

bCardsV :: [Card] -> Box
bCardsV = vcat right . fmap bCard

bPile :: Pile -> Box
bPile (Pile [] []) = text "--"
bPile (Pile hiddens showns) = vcat right (replicate (length hiddens) (text "??")) // bCardsV (reverse showns)

bPiles :: [Pile] -> Box
bPiles piles = hsep 1 top (zipWith (\i pile -> text (show i) // bPile pile) [1..] piles)

bStack :: [Card] -> Box
bStack [] = text "--"
bStack (c:_) = bCard c

bStackHidden :: [Card] -> Box
bStackHidden [] = text "--"
bStackHidden _ = text "??"

bDeck :: Deck -> Box
bDeck = bStackHidden

bDraws :: Draws -> Box
bDraws = bStack

bFoundation :: Foundation -> Box
bFoundation = bStack

bFoundations :: (Foundation, Foundation, Foundation, Foundation) -> Box
bFoundations (a,b,c,d) = hsep 1 top (fmap bFoundation [a,b,c,d])

bGame :: Game -> Box
bGame (Game deck draws foundations piles) =
   vcat left
      [ text "Deck:" <+> bDeck deck
      , text "Draws:" <+> bDraws draws
      , text "Foundations:" <+> bFoundations foundations
      , text "Piles:"
      , bPiles piles
      ]

renderGame :: Game -> String
renderGame = render . bGame
