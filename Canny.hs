{-# LANGUAGE ScopedTypeVariables
,BangPatterns
, FlexibleContexts
, MultiWayIf #-}
import Prelude hiding (filter)
import System.Environment (getArgs)

import Vision.Detector.Edge (canny)
import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)

import Control.Monad (when)
import Control.Monad.ST.Safe (ST)
import Data.Int
import Data.Vector.Storable (enumFromN, forM_)
import Foreign.Storable (Storable)

import Vision.Image (Image, ImagePixel, Manifest, MutableManifest, Grey, DerivativeType (..)
  , (!), shape, linearIndex, fromFunction
  , create, new', linearRead, linearWrite
  , sobel
  )
import Data.List(filter)
import Vision.Primitive (Z (..), (:.) (..), inShape, ix2)

data EdgeDirection = NorthSouth         -- ^ |
                   | WestEast           -- ^ ―
                   | NorthEastSouthWest -- ^ /
                   | NorthWestSouthEast -- ^ \

type Angle = Int

get_angles :: Grey -> [Int]
get_angles !img = do
    x <- [0..(w-1)]
    y <- [0..(h-1)]
    let linearIX = y * w + x
        pdx = dx `linearIndex` linearIX
        pdy = dy `linearIndex` linearIX
    if (pdx,pdy) == (0,0) then []
                          else return . floor $ 100 * atan1 (double pdx) (double pdy)
                          -- note we're going pdx/pdy, so we're looking at horizontal
                          -- stuff
  where 
    size@(Z :. h :. w) = shape img
    dx = sobel 5 DerivativeX img :: Manifest Int16
    dy = sobel 5 DerivativeY img :: Manifest Int16
    atan1 x y = let t = atan2 x y in
                    if t < 0 
                       then t + pi
                       else t
    pi = 3.14159265358979

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double = fromIntegral

main :: IO ()
main = do
    [input, tolerance'] <- getArgs
    let tolerance = Prelude.read tolerance'
    -- Loads the image. Automatically infers the format.
    io <- load Autodetect input

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
                good x = abs (x - 157) < tolerance
            print . length . filter good . get_angles $ edges
