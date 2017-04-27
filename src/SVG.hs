module SVG (
  generatePlayingFieldSVG,
  renderPiece
) where

import Data.List (intersperse)
import qualified Data.Map as Map
import Types

generatePlayingFieldSVG :: PlayingField -> String
generatePlayingFieldSVG pf = header ++ pieces ++ "\n" ++ footer
  where
    header = unlines [
      "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
      "<svg width=\"" ++ show sideLengthX ++ "\" height=\"" ++ show sideLengthY ++ "\">"
      ]
    pieces = concat $ intersperse "\n" $
      map (\(cs, char) -> renderPiece sideLength minXoffset minYoffset cs char) (Map.toList pf)
    footer = "</svg>"
    -- length of one side of a piece in pixels
    sideLength = 50
    (xs, ys) = unzip $ Map.keys pf
    minX = minimum xs
    minY = minimum ys
    maxX = maximum xs
    maxY = maximum ys
    minXoffset = (abs minX - 1) * sideLength
    minYoffset = (abs minY - 1) * sideLength
    sideLengthX = minXoffset + sideLength * (maxX + 2)
    sideLengthY = minYoffset + sideLength * (maxY + 2)

renderPiece :: Int -> Int -> Int -> Coordinates -> Char -> String
renderPiece sideLength minXoffset minYoffset (x,y) char =
  wrapInSvg $ rectangle ++ "\n" ++ shapeText
  where
    startX = show $ minXoffset + x * sideLength
    startY = show $ minYoffset + y * sideLength
    -- TODO: Draw the stroke on the inside?
    rectangle = "<rect x=\"0\" y=\"0\" width=\"" ++ show sideLength ++
                "\" height=\"" ++ show sideLength ++ "\" fill=\"white\"" ++
                " stroke-width=\"1\" stroke=\"black\" stroke-linejoin=\"round\" />"
    shapeText = "<text x=\"60%\" y=\"60%\" alignment-baseline=\"middle\" text-anchor=\"middle\" font-size=\"2em\">"
                ++ [char] ++ "</text>"
    -- this wrapper is necessary to center the text...
    wrapInSvg text = "<svg x=\"" ++ startX ++ "\" y=\"" ++ startY ++ "\" width=\"" ++
                     show sideLength ++ "\" height=\"" ++ show sideLength ++
                     "\">\n" ++ text ++ "\n</svg>"
