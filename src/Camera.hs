{-# LANGUAGE TupleSections #-}
module Camera (
  cameraProducer, cameraArrayProducer
  ) where

import Control.Applicative

import Control.Monad

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO as A

import Data.Maybe

import Data.Vector.Storable as S
--import Data.Word

import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
--import Foreign.Storable

import Pipes

import Prelude as P

import Camera.FFI

--type DIM2 = (Int,Int)

toImage :: DIM3 -> Ptr Word8 -> IO (Array DIM3 Word8)
toImage dim ptr = A.fromPtr dim ptr :: IO (Array DIM3 Word8)

imageSize :: (Int,Int,Int) -> Int
imageSize (zdim,ydim,xdim) = zdim*ydim*xdim

cameraProducer :: Int -> Int -> IO (Maybe (Producer (S.Vector Word8) IO ()))
cameraProducer width height =
  do
    mcfptr <- camera width height
    case mcfptr of
      Nothing -> return Nothing
      Just cfptr ->
        do
          dim <- cameraDim cfptr
          let
            s = imageSize dim
            produce =
              do
                marr <-
                  liftIO $
                  withForeignPtr cfptr
                  (\cptr ->
                     do
                       mptr <- grab cptr
                       maybePeek
                         (\ptr ->
                          do
                              fptr <- newForeignPtr_ ptr
                              return $ S.unsafeFromForeignPtr0 fptr s
                         ) mptr)

                maybe (return ()) yield marr
                produce
          return $ Just produce

cameraDim :: ForeignPtr Camera -> IO (Int,Int,Int)
cameraDim cfptr =
  do
    h <- P.fromIntegral <$> withForeignPtr cfptr getHeight
    w <- P.fromIntegral <$> withForeignPtr cfptr getWidth
    return $ (h,w,3)

toDIM :: (Int,Int,Int) -> DIM3
toDIM (a,b,c) = Z :. a :. b :. c

cameraArrayProducer :: Int -> Int -> IO (Maybe (Producer (Array DIM3 Word8) IO ()))
cameraArrayProducer width height =
  do
    mcfptr <- camera width height
    case mcfptr of
      Nothing -> return Nothing
      Just cfptr ->
        do
          dim <- toDIM <$> cameraDim cfptr
          print dim
          let
            produce =
              do
                marr <-
                  liftIO $
                  withForeignPtr cfptr
                  (\cptr -> grab cptr >>= maybePeek (toImage dim))
                maybe (return ()) yield marr
                produce
            {-# INLINABLE produce #-}
          return $ Just produce
{-# INLINABLE cameraProducer #-}

{-
dim3ToTuple :: Acc (Array DIM3 a) -> Acc (Array DIM2 (a,a,a))
dim3ToTuple arr =
  let
    (w,h,_) = unlift3E $ unindex3 $ A.shape arr
    f :: Exp DIM2 -> Exp (a,a,a)
    f sh =
      let
        (x,y) = unlift2E $ unindex2 sh
        b = arr ! index3 x y 0
        g = arr ! index3 x y 1
        r = arr ! index3 x y 2
      in
        lift3E (b,g,r)
  in
    A.generate (index2 w h) f
-}
