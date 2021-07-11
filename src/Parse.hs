module Parse(parseMove) where

import Move ( Move(..) )
import Text.Parsec
import qualified Text.Parsec.Token as P
import Data.Functor
import Text.Parsec.Language (emptyDef)
import Control.Arrow (left)

type Parser = Parsec String ()

lang :: P.TokenParser ()
lang = P.makeTokenParser emptyDef

natural :: Parser Int
natural = fromInteger <$> P.natural lang

symbol :: String -> Parser String
symbol = P.symbol lang

pDraw :: Parser Move
pDraw = symbol "d" $> Draw

pDrawToFoundation :: Parser Move
pDrawToFoundation = do
    symbol "df"
    DrawToFoundation <$> natural

pDrawToPile :: Parser Move
pDrawToPile = do
    symbol "dp"
    DrawToPile <$> natural

pFoundationToPile :: Parser Move
pFoundationToPile = do
    symbol "fp"
    FoundationToPile <$> natural <*> natural

pPileToFoundation :: Parser Move
pPileToFoundation = do
    symbol "pf"
    PileToFoundation <$> natural <*> natural

pPileToPile :: Parser Move
pPileToPile = do
    symbol "pp"
    PileToPile <$> natural <*> natural <*> natural

pMove :: Parser Move
pMove = spaces *> choice
    [ try pDrawToPile
    , try pDrawToFoundation
    , try pDraw
    , try pFoundationToPile
    , try pPileToFoundation
    , try pPileToPile
    ] <* eof

parseMove :: String -> Either String Move
parseMove = left show . parse pMove ""
