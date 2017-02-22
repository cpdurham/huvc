{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad

import Data.Vector.Storable as S
import Data.Word

import Pipes
import Pipes.Prelude (take)
import Prelude as P

import Camera

main :: IO ()
main = do
  putStrLn "Trying to access first available camera at 1920x1080.."
  mproducer <- cameraProducer 1920 1080
  case mproducer of
    Nothing -> putStrLn "Failed to find a camera"
    Just ((_h,_w,_d),producer) ->
      do
        putStrLn "Found camera, printing first 20 frame data sums"
        runEffect $ producer >-> Pipes.Prelude.take 20 >-> consumer
    
consumer :: Consumer (S.Vector Word8) IO ()
consumer =
  forever $
  do
    v <- await
    liftIO $ print $ (S.sum . S.map toInt) v

toInt :: Word8 -> Int
toInt = fromIntegral
