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

chart organism text = toRenderable layout
  where
    countsC = count 'C' text
    countsG = count 'G' text
    --diffGC :: [(Int, Int)]
    --diffGC  = zip [0..] [x-y | (x, y) <- (zip countsG countsC)]
    diffGC :: [Int]
    diffGC = [x-y | (x, y) <- (zip countsG countsC)]

    plot = plot_lines_values .~ [(zip ([0..]::[Int]) diffGC)]
      $ plot_lines_style . line_color .~ opaque blue
      $ plot_lines_title .~ "#G - #C"
      $ def

    layout = layout_title .~ organism ++ ": Difference in G/C counts."
           $ layout_plots .~ [toPlot plot]
           $ def

--main1 :: [String] -> IO ()
main' :: String -> String -> IO (PickFn ())
main' organism text = renderableToFile def fname $ chart organism text
  where fname = organism ++ ".gcDiff" ++ ".png"

main = do
  args <- getArgs
  text <- readFile (head args)
  putStrLn ("length of text " ++ (show (length text)))
  main' (tail $ snd $ break (\c -> c == '/') (head args)) text
