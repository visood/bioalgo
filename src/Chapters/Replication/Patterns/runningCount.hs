import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import System.Environment(getArgs)

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style . line_color .~ opaque blue

chart args = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque green)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ (head args)
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def

--main1 :: [String] -> IO ()
main' :: [String] -> IO (PickFn ())
main' args = renderableToFile def "example11_big.png" $ chart args

main = do
  args <- getArgs
  main' args
