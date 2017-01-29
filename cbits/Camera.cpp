#include <opencv2/objdetect/objdetect.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include <iostream>

using namespace std;
using namespace cv;

class Camera {
public :
  VideoCapture cap;
  int width;
  int height;
  Mat frame;

  Camera(int _width, int _height) {
    width = _width;
    height = _height;
    cap = VideoCapture(0);
    if(!cap.isOpened()) {
      throw 0;
    }
    cap.set(CV_CAP_PROP_FRAME_WIDTH,width);
    cap.set(CV_CAP_PROP_FRAME_HEIGHT,height);
  }

  ~Camera() {}

  uint32_t getHeight() {
    return height;
  }

  uint32_t getWidth() {
    return width;
  }

  uchar* grab() {
    cap >> frame;
    return frame.data;
  }
};

extern "C" uint32_t Camera_height(Camera* camera) {
    return camera->getHeight();
}

extern "C" uint32_t Camera_width(Camera* camera) {
    return camera->getWidth();
}

extern "C" Camera* Camera_new(int width, int height) {
  try {
    Camera* camera = new Camera(width,height);
    return camera;
  }
  catch (...) {
    return NULL;
  }
  //  return new Camera(width,height);
}

extern "C" void Camera_delete(Camera* camera) {
  if (camera != NULL) {
      delete camera;
    }
}

extern "C" uint8_t* Camera_grab(Camera* camera) {
    return camera->grab();
}
