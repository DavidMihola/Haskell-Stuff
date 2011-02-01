import Text.Printf

type Picture = String


data Point = Point Double Double deriving (Show, Eq)

data SVG = Line {
              lineFrom :: Point,
              lineTo   :: Point
            }
            |
            Rectangle {
              topLeftX :: Double,
              topLeftY :: Double,
              width :: Double,
              height :: Double,
              radiusX :: Double,
              radiusY :: Double,
              color :: (Int,Int,Int)
            }
            |
            Card {
              background :: [SVG],
              topLeft :: [SVG],
              topRight :: [SVG],
              bottomLeft :: [SVG],
              bottomRight :: [SVG],
              center :: [SVG]
            }

            deriving (Show, Eq)

{-
instance Show Card where
  show (Card lo ro lu ru mi) = "LO:" ++ concat lo ++ "\n\
                               \RO:" ++ concat ro ++ "\n\
                               \LU:" ++ concat lu ++ "\n\
                               \RO:" ++ concat ru ++ "\n\
                               \MI:" ++ concat mi ++ "\n"
-}

svgShow (Card bg lo ro lu ru mi) = concat (map svgShow bg) :: String

svgShow (Rectangle x y w h rx ry (r,g,b)) =
    printf "<rect x =\"%.2f\" y =\"%.2f\" width =\"%.2f\" height =\"%.2f\" \
           \rx =\"%.2f\" ry =\"%.2f\" style=\"fill:rgb(%d,%d,%d)\" />" x y w h rx ry r g b:: String
svgShow (Line (Point x1 y1) (Point x2 y2)) = (printf fmtStr x1 y1 x2 y2) :: String
    where fmtStr = "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:black;stroke-width:3;\" />\n"

ppm :: Double
ppm = 3.54

a4X :: Double
a4X = 210.0 * ppm

a4Y :: Double
a4Y = 297.0 * ppm

magicCardX :: Double
magicCardX = 63.0 * ppm

magicCardY :: Double
magicCardY = 88.0 * ppm

pageRows :: Int
pageRows = 3

pageCols :: Int
pageCols = 3

magicCardGrid :: [SVG]
magicCardGrid = hLines ++ vLines
  where hLines = map (\y -> Line (Point 0 y) (Point a4X y)) (take (pageRows + 1) [padY, (padY + magicCardY) ..])
        vLines = map (\x -> Line (Point x 0) (Point x a4Y)) (take (pageCols + 1) [padX, (padX + magicCardX) ..])
        padY = (a4Y - (fromIntegral pageRows) * magicCardY) / 2
        padX = (a4X - (fromIntegral pageCols) * magicCardX) / 2

cp :: [a] -> [a] -> [(a,a)]
cp xs ys = [(x,y) | x <- xs, y <-ys]

magicCardPositions :: [(Double, Double)]
magicCardPositions = cp xs ys
--magicCardPositions = [Point x y | (x,y) <- (cp xs ys)]
  where xs = take pageCols [padX, (padX + magicCardX) ..]
        ys = take pageRows [padY, (padY + magicCardY) ..]
        padY = (a4Y - (fromIntegral pageRows) * magicCardY) / 2
        padX = (a4X - (fromIntegral pageCols) * magicCardX) / 2

magicCardBackground :: SVG
magicCardBackground = Rectangle 0 0 magicCardX magicCardY 0 0 (0,0,0)

magicCardBorder :: SVG
magicCardBorder = Rectangle borderX borderY w h rad rad (255,255,255)
  where w = magicCardX - 2 * borderX
        h = magicCardY - 2 * borderY
        borderX = 6
        borderY = 6
        rad = 12

emptyCard :: SVG
emptyCard = Card [] [] [] [] [] []

magicCard :: SVG
magicCard = emptyCard {background = [magicCardBackground, magicCardBorder]}

addBackground :: SVG -> SVG -> SVG
addBackground card svg = card {background = old ++ [svg]}
  where old = background card

atPositions :: [(Double,Double)] -> [SVG] -> String
atPositions ps cs = concat xs
  where xs = zipWith (\(x,y) c -> printf "<g transform=\"translate(%.2f, %.2f)\">\n%s\n</g>" x y c) ps (map svgShow cs)
