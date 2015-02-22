{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (filter)
import System.Environment (getArgs)

import Vision.Detector.Edge (canny)
import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (Z (..), (:.) (..), inShape, ix2)


import Data.Maybe (maybeToList)
import Data.List (filter)
--colorHist :: -> []

mask :: Grey -> RGB -> DelayedMask RGBPixel
mask maskimg img = DelayedMask size g
  where
    g p = if maskimg ! p == 255 
             then Just $ img ! p 
             else Nothing
    size = shape maskimg
    
get_colors :: DelayedMask RGBPixel -> [RGBPixel]
get_colors img = do
    x <- [0..(w-1)]
    y <- [0..(h-1)]
    let linearIX = y * w + x
    maybeToList $ img `maskedLinearIndex` linearIX
  where 
    size@(Z :. h :. w) = shape img
    

main :: IO ()
main = do
    [input, output] <- getArgs
    -- Loads the image. Automatically infers the format.
    io <- load Autodetect input
    io2 <- load Autodetect input

    case io of
        Left err             -> do
            putStrLn "Unable to load the image:"
            print err
        Right (grey :: Grey) -> do
            let blurred, edges :: Grey
                -- Applies a Gaussian filter with a 3x3 Double kernel to remove
                -- small noises.
                blurred = gaussianBlur 1 (Nothing :: Maybe Double) grey

                -- Applies the Canny's algorithm with a 5x5 Sobel kernel (radius
                -- = 2).
                edges = canny 2 256 1024 blurred
            let cannyblurred = (gaussianBlur 5 (Nothing :: Maybe Double) edges)::Grey
            let thresheld = (threshold (/= 0) (BinaryThreshold (255::GreyPixel) 0) cannyblurred)::Grey
            case io2 of
                 Right (color :: RGB) -> do 
                  let colors = get_colors (mask thresheld color)
                      good (RGBPixel r g b) = (r > 240) && (g > 240) && (b > 240)
                  print . length . filter good $ colors
                  mErr <- save Autodetect output grey
                  return ()