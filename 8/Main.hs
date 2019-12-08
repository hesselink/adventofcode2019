import Data.List.Split (chunksOf)
import Data.Char (digitToInt, isDigit)
import Data.Ord (comparing)
import Data.List (minimumBy, intercalate)

main :: IO ()
main = do
  f <- readFile "input/8"
  let image = parseImage 25 6 f
      layer = layerWithLeastZeroes image
  print (oneDigitsTimesTwoDigits layer)
  putStrLn $ showImage image

type Row = [Int]
type Layer = [Row]
type Image = [Layer]

parseImage :: Int -> Int -> String -> Image
parseImage w h = chunksOf h . chunksOf w . map digitToInt . filter isDigit

numberOf :: Int -> Layer -> Int
numberOf n = length . filter (== n) . concat

layerWithLeastZeroes :: Image -> Layer
layerWithLeastZeroes = minimumBy (comparing (numberOf 0))

oneDigitsTimesTwoDigits :: Layer -> Int
oneDigitsTimesTwoDigits l = numberOf 1 l * numberOf 2 l

mergeLayers :: Image -> Layer
mergeLayers = foldr1 mergeLayer

mergeLayer :: Layer -> Layer -> Layer
mergeLayer = zipWith (zipWith mergePixel)

mergePixel :: Int -> Int -> Int
mergePixel 2 y = y
mergePixel x _ = x

showPixel :: Int -> Char
showPixel 0 = ' '
showPixel 1 = '#'
showPixel 2 = ' '
showPixel _ = 'E'

showLayer :: Layer -> String
showLayer = intercalate "\n" . map (map showPixel)

showImage :: Image -> String
showImage = showLayer . mergeLayers
