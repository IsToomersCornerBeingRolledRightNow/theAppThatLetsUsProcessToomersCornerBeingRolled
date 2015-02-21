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
import Data.List(sort,group,sortBy,filter)
import Data.Ord (comparing, Down(..))
import Vision.Primitive (Z (..), (:.) (..), inShape, ix2)

data EdgeDirection = NorthSouth         -- ^ |
                   | WestEast           -- ^ ―
                   | NorthEastSouthWest -- ^ /
                   | NorthWestSouthEast -- ^ \

-- | Detects edges using the Canny's algorithm. Edges are given the value
-- 'maxBound' while non-edges are given the value 'minBound'.
--
-- This implementation doesn't perform any noise erasing (as blurring) before
-- edge detection. Noisy images might need to be pre-processed using a Gaussian
-- blur.
--
-- The bidirectional derivative (gradient magnitude) is computed from @x@ and
-- @y@ derivatives using @sqrt(dx² + dy²)@.
--
-- See <http://en.wikipedia.org/wiki/Canny_edge_detector> for details.
--
-- This function is specialized for 'Grey' images but is declared @INLINABLE@
-- to be further specialized for new image types.
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
  where 
    size@(Z :. h :. w) = shape img
    dx = sobel 5 DerivativeX img :: Manifest Int16
    dy = sobel 5 DerivativeY img :: Manifest Int16
    atan1 x y = let t = atan2 x y in
                    if t < 0 
                       then t + pi
                       else t
    pi = 3.14159265358979
canny_dx ::
      Int
      -- ^ Radius of the Sobel's filter.
      -> Int32
      -- ^ Low threshold. Pixels for which the bidirectional derivative is
      -- greater than this value and which are connected to another pixel which
      -- is part of an edge will be part of this edge.
      -> Int32
      -- ^ High threshold. Pixels for which the bidirectional derivative is
      -- greater than this value will be part of an edge.
      -> Grey
      -> Grey
canny_dx !derivSize !lowThres !highThres !img =
    create $ do
        edges <- newManifest
        forM_ (enumFromN 0 h) $ \y -> do
            let !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                visitPoint edges x y (lineOffset + x) highThres'
        return edges
  where
    !size@(Z :. h :. w) = shape img

    -- Squares both thresholds as they will be compared to 'dxy' which contains
    -- squared gradient magnitudes.
    (!lowThres', !highThres') = (square lowThres, square highThres)

    dx, dy :: Manifest GreyPixel
    !dx = sobel derivSize DerivativeX img
    !dy = sobel derivSize DerivativeY img

    -- Gradient magnitude, squared.
    dxy :: Manifest Int32
    !dxy = fromFunction size $ \pt ->
                  square (fromIntegral $ dx ! pt)
                + square (fromIntegral $ dy ! pt)
    newManifest :: (Storable p, Bounded p) => ST s (MutableManifest p s)
    newManifest = new' size minBound
    -- Visits a point and compares its gradient magnitude to the given
    -- threshold, visits neighbor if the point is perceived an an edge.
    visitPoint !img !x !y !linearIX _ = -- return $ dx  `linearIndex` linearIX
      --linearWrite img linearIX $ dx  `linearIndex` linearIX
      linearWrite img linearIX $ angle
        where
          ptdx = double (dx `linearIndex` linearIX)
          ptdy = double (dy `linearIndex` linearIX)
          angle = floor $ (atan (ptdx/ptdy) + pi/2)/pi * 255
    {-visitPoint !edges !x !y !linearIX !thres = do
        val <- linearRead edges linearIX

        when (val == minBound) $ do
            let !ptDxy    = dxy `linearIndex` linearIX
                ptDx      = dx  `linearIndex` linearIX
                ptDy      = dy  `linearIndex` linearIX
                direction = edgeDirection ptDx ptDy

            -- When the current pixel has a greater magnitude than the threshold
            -- and is a local maximum, considers it as a new starting point of
            -- an edge. Tries to draw the remaining of the edge using the low
            -- threshold and by following the edge direction.

            when (ptDxy > thres && isMaximum x y ptDxy direction) $ do
                linearWrite edges linearIX maxBound
                visitNeighbour edges x y direction-}
    visitNeighbour !edges !x !y !direction = do
        let (!x1, !y1, !x2, !y2) =
                case direction of
                    NorthSouth         -> (x,     y - 1, x,     y + 1)
                    WestEast           -> (x - 1, y,     x + 1, y    )
                    NorthEastSouthWest -> (x - 1, y - 1, x + 1, y + 1)
                    NorthWestSouthEast -> (x + 1, y - 1, x - 1, y + 1)

        when (inShape size (ix2 y1 x1)) $
            visitPoint edges x1 y1 (y1 * w + x1) lowThres'

        when (inShape size (ix2 y2 x2)) $
            visitPoint edges x2 y2 (y2 * w + x2) lowThres'

    isMaximum !x !y !ptDxy !direction =
        let (!x1, !y1, !x2, !y2) =
                case direction of
                    NorthSouth         -> (x - 1, y,     x + 1, y    )
                    WestEast           -> (x,     y - 1, x,     y + 1)
                    NorthEastSouthWest -> (x + 1, y - 1, x - 1, y + 1)
                    NorthWestSouthEast -> (x - 1, y - 1, x + 1, y + 1)
        in tryCompare ptDxy (>) (x1, y1) && tryCompare ptDxy (>=) (x2, y2)

    tryCompare !ptDxy op !(x, y)
        | inShape size (ix2 y x) = ptDxy `op` fromIntegral (dxy ! ix2 y x)
        | otherwise              = True

    -- Returns the direction of the edge, not to be confused with the direction
    -- of the gradient which is the perpendicular of this value.
    edgeDirection ptDx ptDy =
        let !angle = atan2 (double ptDy) (double ptDx)
        in if angle >= 0 then if | angle >  pi8x7 -> NorthSouth
                                 | angle >  pi8x5 -> NorthEastSouthWest
                                 | angle >  pi8x3 -> WestEast
                                 | angle >    pi8 -> NorthWestSouthEast
                                 | otherwise      -> NorthSouth
                         else if | angle < -pi8x7 -> NorthSouth
                                 | angle < -pi8x5 -> NorthWestSouthEast
                                 | angle < -pi8x3 -> WestEast
                                 | angle <   -pi8 -> NorthEastSouthWest
                                 | otherwise      -> NorthSouth

    !pi8   = pi / 8
    !pi8x3 = pi8 * 3
    !pi8x5 = pi8 * 5
    !pi8x7 = pi8 * 7

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double = fromIntegral

 -- Detects the edge of the image with the Canny's edge detector.
--
-- usage: ./canny input.png output.png
main :: IO ()
main = do
    [input] <- getArgs

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
                hist xs = sortBy (comparing (Down . snd)) . fmap (\x -> (head x, length x)) . group . sort $ xs
                good x = abs (x - 157) < 10
            print . length . filter good . get_angles $ edges
            --    thetas = get_angles edges
            --print "aaa"
            --print . length $ thetas
            -- Saves the edges image. Automatically infers the output format.
            --mErr <- save Autodetect output1 edges
            {-case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err-}
