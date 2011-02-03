import Text.Printf
import System.IO

type Picture = String

data Point = Point Double Double deriving (Show, Eq)

data SVGDocument = SVGDocument String

data SVG = Line {
              lineFrom :: Point,
              lineTo   :: Point
            }
            |
            Rectangle {
              svgtopLeftX :: Double,
              svgtopLeftY :: Double,
              svgwidth :: Double,
              svgheight :: Double,
              radiusX :: Double,
              radiusY :: Double,
              color :: (Int,Int,Int)
            }
            |
            Group {
              svgwidth :: Double,
              svgheight :: Double,
              content :: String
            }
            deriving (Show, Eq)

data Card = Card {
              background :: [SVG],
              topLeft :: [SVG],
              topRight :: [SVG],
              bottomLeft :: [SVG],
              bottomRight :: [SVG],
              center :: [SVG]
            }

data PageDef = PageDef {
                 pageWidth :: Double,
                 pageHeight :: Double
               }

data CardDef = CardDef {
                 cardWidth :: Double,
                 cardHeight :: Double,
                 topLeftX :: Double,
                 topLeftY :: Double,
                 topRightX :: Double,
                 topRightY :: Double,
                 bottomLeftX :: Double,
                 bottomLeftY :: Double,
                 bottomRightX :: Double,
                 bottomRightY :: Double,
                 centerX :: Double,
                 centerY :: Double
               } deriving (Show)

basicCardDef :: Double -> Double -> Double -> CardDef
basicCardDef w h b = CardDef {cardWidth = w, cardHeight = h, topLeftX = b, topLeftY = b,
                              topRightX = w - b, topRightY = b, bottomLeftX = b, bottomLeftY = h - b,
                              bottomRightX = w - b, bottomRightY = h - b, centerX = w / 2, centerY = h / 2}

magicCardDef :: CardDef
magicCardDef = basicCardDef (63.0 * ppm) (88.0 * ppm) (10 * ppm)

class SVGable a where
  svgShow :: a -> String

instance SVGable Card where
  svgShow (Card bg lo ro lu ru mi) = concat (map svgShow bg) :: String

instance SVGable SVG where
  svgShow (Rectangle x y w h rx ry (r,g,b)) =
    printf "<rect x =\"%.2f\" y =\"%.2f\" width =\"%.2f\" height =\"%.2f\" \
           \rx =\"%.2f\" ry =\"%.2f\" style=\"fill:rgb(%d,%d,%d)\" />" x y w h rx ry r g b:: String
  svgShow (Line (Point x1 y1) (Point x2 y2)) = (printf fmtStr x1 y1 x2 y2) :: String
    where fmtStr = "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:black;stroke-width:1;\" />\n"

instance SVGable SVGDocument where
  svgShow (SVGDocument c) = (printf fmtStr c) :: String
    where fmtStr = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
                   \<svg\n\
                   \xmlns:svg=\"http://www.w3.org/2000/svg\"\n\
                   \xmlns=\"http://www.w3.org/2000/svg\"\n\
                   \version=\"1.1\"\n\
                   \width=\"744.09448\"\n\
                   \height=\"1052.3622\"\n\
                   \id=\"svg2\">\n\
                   \<defs\n\
                   \id=\"defs4\" />\n\
                   \<g\n\
                   \id=\"layer1\" >\n\
	           \%s\n\
                   \</g>\n\
                   \</svg>"

ppm :: Double
ppm = 3.54

a4Def :: PageDef
a4Def = PageDef (210 * ppm) (297 * ppm)

cardGrid :: PageDef -> CardDef -> [SVG]
cardGrid page card = hLines ++ vLines
  where pw = pageWidth page
        ph = pageHeight page
        cw = cardWidth card
        ch = cardHeight card
        rows = floor (ph / ch)
        cols = floor (pw / cw)
        padX = (pw - (fromIntegral cols) * cw) / 2
        padY = (ph - (fromIntegral rows) * ch) / 2
        hLines = map (\y -> Line (Point 0 y) (Point pw y)) (take (rows + 1) [padY, (padY + ch) ..])
        vLines = map (\x -> Line (Point x 0) (Point x ph)) (take (cols + 1) [padX, (padX + cw) ..])

magicCardGrid :: [SVG]
magicCardGrid = cardGrid a4Def magicCardDef

cardPositions :: PageDef -> CardDef -> [(Double, Double)]
cardPositions page card = cp xs ys
  where pw = pageWidth page
        ph = pageHeight page
        cw = cardWidth card
        ch = cardHeight card
        rows = floor (ph / ch)
        cols = floor (pw / cw)
        padX = (pw - (fromIntegral cols) * cw) / 2
        padY = (ph - (fromIntegral rows) * ch) / 2
        xs = take cols [padX, (padX + cw) ..]
        ys = take cols [padY, (padY + ch) ..]

cp :: [a] -> [a] -> [(a,a)]
cp xs ys = [(x,y) | x <- xs, y <-ys]

magicCardPositions :: [(Double, Double)]
magicCardPositions = cardPositions a4Def magicCardDef

magicCardBackground :: SVG
magicCardBackground = Rectangle 0 0 cw ch 0 0 (0,0,0)
  where cw = cardWidth magicCardDef
        ch = cardHeight magicCardDef

magicCardBorder :: SVG
magicCardBorder = Rectangle borderX borderY w h rad rad (255,255,255)
  where cw = cardWidth magicCardDef
        ch = cardHeight magicCardDef
        w = cw - 2 * borderX
        h = ch - 2 * borderY
        borderX = 6
        borderY = 6
        rad = 12

emptyCard :: Card
emptyCard = Card [] [] [] [] [] []

magicCard :: Card
magicCard = emptyCard {background = [magicCardBackground, magicCardBorder]}

addBackground :: Card -> SVG -> Card
addBackground card svg = card {background = old ++ [svg]}
  where old = background card

addTopLeft :: Card -> SVG -> Card
addTopLeft card svg = card {topLeft = old ++ [svg]}
  where old = topLeft card

addTopRight :: Card -> SVG -> Card
addTopRight card svg = card {topRight = old ++ [svg]}
  where old = topRight card

addBottomLeft :: Card -> SVG -> Card
addBottomLeft card svg = card {bottomLeft = old ++ [svg]}
  where old = bottomLeft card

addBottomRight :: Card -> SVG -> Card
addBottomRight card svg = card {bottomRight = old ++ [svg]}
  where old = bottomRight card

addCenter :: Card -> SVG -> Card
addCenter card svg = card {center = old ++ [svg]}
  where old = center card
	
--

{-
Schade, dass das hier offenbar nicht geht:

addElement :: (Card -> [SVG]) -> SVG -> Card -> Card
addElement pos svg card = card {pos = old ++ [svg]}
  where old = pos card

addBackground = addElement background

etc.
-}

groupAtPosition :: SVGable a => (Double,Double) -> a -> String
groupAtPosition (x,y) c = printf "<g transform=\"translate(%.2f, %.2f)\">\n%s\n</g>" x y (svgShow c)

groupsAtPositions :: SVGable a => [(Double, Double)] -> [a] -> String
groupsAtPositions ps cs = concat $ zipWith groupAtPosition ps cs

addContent :: SVGDocument -> String -> SVGDocument
addContent (SVGDocument old) str = SVGDocument (old ++ str)

gridPage = SVGDocument (concatMap svgShow magicCardGrid)

cardPage = addContent gridPage (groupsAtPositions magicCardPositions (repeat magicCard))

main :: IO ()
main = do
       outh <- openFile "output.svg" WriteMode
       hPutStr outh (svgShow cardPage)
       hClose outh
