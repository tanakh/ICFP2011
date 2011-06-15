#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <pthread.h>

#include "ofMain.h"
#include "testApp.h"
#include "ofAppGlutWindow.h"

using namespace std;



//========================================================================
int main( ){

  ofAppGlutWindow window;
  // <-------- setup the GL context
  ofSetupOpenGL(&window, 800,700, OF_WINDOW);
  
  // create a communication thread.
  pthread_t thread;
  pthread_create(&thread, NULL, inputter ,(void*) NULL);

  // this kicks off the running of my app
  // can be OF_WINDOW or OF_FULLSCREEN
  // pass in width and height too:
  ofRunApp( new testApp());
  pthread_detach(thread);
}
