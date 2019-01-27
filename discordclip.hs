module Main where

import System.Environment
import Codec.Picture
import Data.List
import Data.Maybe
import Control.Monad

type Screenshot = Image PixelRGB8

bg :: PixelRGB8
bg = PixelRGB8 0x36 0x39 0x3f

dv :: PixelRGB8
dv = PixelRGB8 0x3E 0x41 0x47


leftBorder :: Screenshot -> Int
leftBorder img = 
  let isBgStart x = elem bg [pixelAt img x y | y <- [0 .. imageHeight img - 1]]
      Just border = find isBgStart [0 .. imageWidth img - 1]
  in border

rightBorder :: Screenshot -> Int
rightBorder img = 
  let isBgStart x = elem bg [pixelAt img x y | y <- [0 .. imageHeight img - 1]]
      Just border = find isBgStart [imageWidth img - 1, imageWidth img - 2 ..]
  in border

topBorder :: Screenshot -> Int
topBorder img = 
  let isBgStart y = elem bg [pixelAt img x y | x <- [0 .. imageWidth img - 1]]
      Just border = find isBgStart [0 .. imageHeight img - 1]
  in border

bottomBorder :: Screenshot -> Int
bottomBorder img = 
  let isBgStart y = elem bg [pixelAt img x y | x <- [0 .. imageWidth img - 1]]
      Just border = find isBgStart [imageHeight img - 1, imageHeight img - 2 .. 0]
  in border

leftEdge :: Screenshot -> Int
leftEdge img =
  let isBgEnd x = any (not . (== bg)) [pixelAt img x y | y <- [0 .. imageHeight img - 1]]
      Just edge = find isBgEnd [0 .. imageWidth img - 1]
  in edge

rightEdge :: Screenshot -> Int
rightEdge img =
  let isBgEnd x = any (not . (== bg)) [pixelAt img x y | y <- [0 .. imageHeight img - 1]]
      Just edge = find isBgEnd [imageWidth img - 1, imageWidth img - 2 ..]
  in edge

topEdge :: Screenshot -> Int
topEdge img =
  let isBgEnd y = any (not . (== bg)) [pixelAt img x y | x <- [0 .. imageWidth img - 1]]
      Just edge = find isBgEnd [0 .. imageHeight img - 1]
  in edge

bottomEdge :: Screenshot -> Int
bottomEdge img =
  let isBgEnd y = any (not . (== bg)) [pixelAt img x y | x <- [0 .. imageWidth img - 1]]
      Just edge = find isBgEnd [imageHeight img - 1, imageHeight img - 2 ..]
  in edge

removeDividers :: Screenshot -> Screenshot
removeDividers img =
  generateImage
    (\x y ->
      case pixelAt img x y of
        p | p == dv -> bg
        p -> p)
    (imageWidth img)
    (imageHeight img)

crop :: Int -> Int -> Int -> Int -> Screenshot -> Screenshot
crop left right top bottom img =
  generateImage
    (\x y -> pixelAt img (x + left) (y + top))
    (right - left + 1)
    (bottom - top + 1)

cropScreenshot :: Screenshot -> Screenshot
cropScreenshot img =
  let borderless =
        crop
          (leftBorder img)
          (rightBorder img)
          (topBorder img)
          (bottomBorder img)
          img
      edgeless =
        crop
          (leftEdge borderless)
          (rightEdge borderless)
          (topEdge borderless)
          (bottomEdge borderless)
          borderless
 in edgeless

padScreenshot :: Screenshot -> Screenshot
padScreenshot img =
  let ypad = 22
      lpad = 16
      w = 1020
      h = imageHeight img + ypad * 2
  in generateImage
       (\x y ->
         if y >= ypad && y < (h - ypad) && x >= lpad && x < imageWidth img + lpad
           then pixelAt img (x - lpad) (y - ypad)
           else bg)
       w
       h

processScreenshot :: Screenshot -> Screenshot
processScreenshot = padScreenshot . cropScreenshot . removeDividers

groupScreenshots :: [Screenshot] -> [[Screenshot]]
groupScreenshots imgs =
  let maxHeight = 2040
      psi ([], []) = Nothing
      psi (grp, []) = Just (reverse grp, ([], []))
      psi (grp, imgs)
        | sum (map imageHeight grp) >= maxHeight = Just (reverse (tail grp), ([], head grp : imgs))
      psi (grp, (i:imgs)) = psi (i : grp, imgs)
  in unfoldr psi ([], imgs)

recombineScreenshots :: [Screenshot] -> Screenshot
recombineScreenshots imgs =
  let (_, taggedImgs) = mapAccumL (\h i -> (h + imageHeight i, (h, i))) 0 imgs
      imgFor y = fromJust (find (\(h, i) -> y >= h) (reverse taggedImgs))
  in generateImage
      (\x y ->
        let (oy, img) = imgFor y
        in pixelAt img x (y - oy))
      (maximum (map imageWidth imgs))
      (sum (map imageHeight imgs))

processScreenshotGroup :: [Screenshot] -> [Screenshot]
processScreenshotGroup = fmap recombineScreenshots . groupScreenshots . fmap processScreenshot

readScreenshots :: [String] -> IO [Screenshot]
readScreenshots =
  traverse
    (\path -> do
        Right i <- readImage path
        pure (convertRGB8 i))

writeScreenshots :: String -> [Screenshot] -> IO ()
writeScreenshots path imgs =
  forM_ (zip [0..] imgs) $ \(i, img) ->
    writePng (path ++ "/msg-" ++ (if i < 10 then "0" else "") ++ show i ++ ".png") img

main :: IO ()
main = do
  (output : input) <- getArgs
  screenshots <- readScreenshots input
  writeScreenshots output (processScreenshotGroup screenshots)
