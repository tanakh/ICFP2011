#include <fstream>
#include <map>
#include <pthread.h>

#include "testApp.h"

using namespace std;


nfWorld *bgWorld;    // the world under construction from the stdin.
nfWorld *fgWorld;    // the world ready for next draw.
nfWorld *drawnWorld; // the world drawn at the moment.

pthread_mutex_t worldMutex; // lock when swapping worlds.
pthread_mutex_t dictMutex; // lock when touching patternDict or filenameDict.

map<string, int> patternDict; // filename to pattern-id dictionary.
vector<string> filenameDict;  // pattern-id to filename dictionary.
vector<ofImage*> images; // archive of loaded image data.


//--------------------------------------------------------------
// convert filename to unique pattern-id.
// add the filename-id pair to the dictionary if needed.
int filenameToPattern (const string &fn) {
  if (patternDict.count(fn) > 0) return patternDict[fn];
  const int ret = filenameDict.size();
  pthread_mutex_lock(&dictMutex); {
    patternDict[fn] = ret;
    filenameDict.push_back(fn);
  } pthread_mutex_unlock(&dictMutex);
  return ret;
}

//========================================================================
// communication 
void* inputter (void* args) {
  int worldIndex = 0;
  const float scale = ofGetHeight();
  
  for (;;) {
    string linestr, cmd;
    getline(cin, linestr);
    istringstream iss(linestr);
    iss >> cmd;
    if (cmd == "") {
      continue;

    } else if (cmd == "flush") {
      // stage the bgWorld ready for drawing.
      pthread_mutex_lock(&worldMutex); {
        swap(fgWorld, bgWorld);
      } pthread_mutex_unlock(&worldMutex);
      bgWorld->clear();
      bgWorld->generation = ++worldIndex;
      
    } else if (cmd == "label") {
      // draw a line of text 
      float x,y,fontsize; string text;
      iss >> x >> y >> fontsize;
      getline(iss, text);
      bgWorld->stamps.push_back
        (nfStamp(x*scale,y*scale,0,-1,fontsize,text));
      
    } else if (cmd == "stamp") {
      // draw an image
      float x,y,zoom; string filename;
      iss >> x >> y >> zoom >> filename;
      int pattern = filenameToPattern(filename);
      bgWorld->stamps.push_back
        (nfStamp(x*scale,y*scale,zoom,pattern));
      
    } else if (cmd == "draw") {
      // draw an image with text on top of it
      float x,y,zoom; string filename; float fontsize; string text;
      iss >> x >> y >> zoom >> filename >> fontsize;
      getline(iss, text);
      int pattern = filenameToPattern(filename);
      bgWorld->stamps.push_back
        (nfStamp(x*scale,y*scale,zoom,pattern,fontsize, text));
      
    } else {
      cerr << "unknown command : " << linestr << endl;
    }
  }
}


//--------------------------------------------------------------
void testApp::setup(){	 
  // load a font with extended parameters:
  // font name, size, anti-aliased, full character set
  verdana.loadFont("verdana.ttf",8, false, true);
  verdana.setLineHeight(20.0f);

  // create worlds
  fgWorld = new nfWorld();
  bgWorld = new nfWorld();
  drawnWorld = new nfWorld();

  // create mutices
  pthread_mutex_init(&worldMutex, NULL);
  pthread_mutex_init(&dictMutex, NULL);
}


//--------------------------------------------------------------
void testApp::update(){
  ofBackground(255,255,255);	
}

//--------------------------------------------------------------
void testApp::draw(){

  // fetch the fgWorld, only if it's newer than the current one
  pthread_mutex_lock(&worldMutex); {
    if (fgWorld->generation > drawnWorld->generation)
      swap(fgWorld, drawnWorld);
  } pthread_mutex_unlock(&worldMutex);

  // load images that are new in dictionary
  pthread_mutex_lock(&dictMutex); {
    while (filenameDict.size() > images.size()) {
      int i = images.size();
      images.push_back(new ofImage());
      images[i]->loadImage(filenameDict[i]);
    }
  } pthread_mutex_unlock(&dictMutex);

  const vector<nfStamp> &stamps = drawnWorld->stamps;
  ofSetColor(0xFFFFFF);

  // draw images
  ofEnableAlphaBlending(); {
    for (int i = 0; i < stamps.size(); ++i) {
      const nfStamp &stamp = stamps[i];
      if (stamp.pattern < 0) continue;
      ofImage *image = images[stamp.pattern];
      ofPushMatrix(); {
        const float w = image->getWidth();
        const float h = image->getHeight();
        const float zoom = stamp.zoom;
        ofTranslate(stamp.x -w*zoom/2, stamp.y -h*zoom/2, 0);
        ofScale(zoom, zoom, 1);
        image->draw(0,0);
      } ofPopMatrix();
    }
  } ofDisableAlphaBlending();

  // draw labels
  for (int i = 0; i < stamps.size(); ++i) {
    const nfStamp &stamp = stamps[i];
    if (stamp.fontsize <= 0) continue;
    ofPushMatrix(); {
      float x = stamp.x, y = stamp.y;
      if (stamp.pattern >= 0) {
        ofImage *image = images[stamp.pattern];
        const float w = image->getWidth();
        const float h = image->getHeight();
        const float zoom = stamp.zoom;
        x -= w*zoom/2;
        y -= h*zoom/2;
      }
      ofTranslate(x, y, 0);
      ofSetColor(0x000000);
      ofScale(stamp.fontsize, stamp.fontsize, 1);
      verdana.drawString(stamp.text,0,0);
    } ofPopMatrix();
  }
}


//--------------------------------------------------------------
void testApp::keyPressed  (int key){ 
	
}

//--------------------------------------------------------------
void testApp::keyReleased(int key){ 
	
}

//--------------------------------------------------------------
void testApp::mouseMoved(int x, int y ){
	
}

//--------------------------------------------------------------
void testApp::mouseDragged(int x, int y, int button){
	
}

//--------------------------------------------------------------
void testApp::mousePressed(int x, int y, int button){
	
}

//--------------------------------------------------------------
void testApp::mouseReleased(int x, int y, int button){

}

//--------------------------------------------------------------
void testApp::windowResized(int w, int h){

}
