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
    diffGC :: [(Int, Int)]
    diffGC  = zip [0..] [x-y | (x, y) <- (zip countsG countsC)]
    xysC :: [(Int, Int)]
    xysC =  zip [0..] countsC
    xysG :: [(Int, Int)]
    xysG =  zip [0..] countsG

    plot1  = plot_points_style .~ filledCircles 2 (opaque green)
      $ plot_points_values .~ xysC
      $ plot_points_title .~ "C"
      $ def

    plot2  = plot_points_style .~ filledCircles 2 (opaque red)
      $ plot_points_values .~ xysG
      $ plot_points_title .~ "G"
      $ def

    plot3  = plot_points_style .~ filledCircles 2 (opaque blue)
      $ plot_points_values .~ diffGC
      $ plot_points_title .~ "G - C"
      $ def

    layout = layout_title .~ "GC counts"
           $ layout_plots .~ [toPlot plot3]
           $ def

--main1 :: [String] -> IO ()
main' :: String -> IO (PickFn ())
main' text = renderableToFile def "gcDiff.png" $ chart text

main = do
  args <- getArgs
  text <- readFile (head args)
  putStrLn ("length of text " ++ (show (length text)))
  main' text
