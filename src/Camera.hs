{-# LANGUAGE TupleSections #-}
module Camera (
  cameraProducer
  ) where

import Control.Applicative

import Control.Monad

import Data.Maybe

import Data.Vector.Storable as S
import Data.Vector.Storable.Mutable as SM

import Data.Word

import Foreign.ForeignPtr
import Foreign.Marshal.Utils

import Pipes

import Prelude as P

import Camera.FFI

type ImageDim = (Int,Int,Int)

imageSize :: ImageDim -> Int
imageSize (zdim,ydim,xdim) = zdim*ydim*xdim

cameraProducer :: Int -> Int -> IO (Maybe (ImageDim, Producer (S.Vector Word8) IO ()))
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
            producer =
              forever $ do
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
                              mvector <- SM.new s
                              let v = S.unsafeFromForeignPtr0 fptr s
                              S.unsafeCopy mvector v
                              S.unsafeFreeze mvector
                         ) mptr)

                maybe (return ()) yield marr
          return $ Just ((height,width,3),producer)
{-# INLINABLE cameraProducer #-}

cameraDim :: ForeignPtr Camera -> IO ImageDim
cameraDim cfptr =
  do
    h <- P.fromIntegral <$> withForeignPtr cfptr getHeight
    w <- P.fromIntegral <$> withForeignPtr cfptr getWidth
    return $ (h,w,3)
