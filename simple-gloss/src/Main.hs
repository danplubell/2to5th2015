module Main where
import           Graphics.Gloss

filename:: [Char]
filename = "FirstHand.bmp"

main :: IO ()
main = do
  picture@(Bitmap width height _ _ ) <- loadBMP filename
  display (InWindow "FirstHand" (width,height) (100,100)) black picture

