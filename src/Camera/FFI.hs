module Camera.FFI where

import Data.Word (Word8,Word32)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (maybePeek)

foreign import ccall unsafe "Camera_new" c_new
  :: Int -> Int -> IO (Ptr Camera)
foreign import ccall unsafe "Camera_height" c_getHeight
  :: Ptr Camera -> IO Word32
foreign import ccall unsafe "Camera_width" c_getWidth
  :: Ptr Camera -> IO Word32
foreign import ccall unsafe "Camera_grab" c_grab
  :: Ptr Camera -> IO (Ptr Word8)
foreign import ccall unsafe "&Camera_delete" c_delete
  :: FinalizerPtr Camera

data Camera

camera :: Int -> Int -> IO (Maybe (ForeignPtr Camera))
camera width height =
  do
    cameraPtr <- c_new width height
    maybePeek (newForeignPtr c_delete) cameraPtr

getHeight :: Ptr Camera -> IO Word32
getHeight = c_getHeight

getWidth :: Ptr Camera -> IO Word32
getWidth = c_getWidth

grab :: Ptr Camera -> IO (Ptr Word8)
grab = c_grab
