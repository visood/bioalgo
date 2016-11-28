import Dna.Dna
import Dna.Kmer
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import System.Environment(getArgs)

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style . line_color .~ opaque blue

count :: Char -> String -> [Int]
count x ys = countWithAcc 0 ys
  where
    countWithAcc :: Int -> String -> [Int]
    countWithAcc _ [] = []
    countWithAcc c (y:ys) = if (y == x)
                              then (c + 1) : (countWithAcc (c + 1) ys)
                              else c : (countWithAcc c ys)
  
chart text = toRenderable layout
  where
    countsC = count 'C' text
    countsG = count 'G' text
    diffGC  = [x-y | x <- countsG, y <- countsC]
    xys :: [(Int, Int)]
    xys     =  [ (x,y) | (x,y) <- zip (take (length text) [0..]) diffGC ]
    plot1   = plot_points_style .~ filledCircles 2 (opaque green)
      $ plot_points_values .~ xys
      $ plot_points_title .~ "gc diff points"
      $ def

    layout = layout_title .~ "GC difference"
           $ layout_plots .~ [toPlot plot1]
           $ def

--main1 :: [String] -> IO ()
main' :: String -> IO (PickFn ())
main' text = renderableToFile def "gcDiff.png" $ chart text

main = do
  args <- getArgs
  text <- readFile (head args)
  main' text
