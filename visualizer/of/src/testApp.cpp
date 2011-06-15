#include <fstream>
#include <map>
#include <pthread.h>

#include "testApp.h"

using namespace std;


nfWorld *fgWorld, *bgWorld, *drawnWorld;
pthread_mutex_t worldMutex;
pthread_mutex_t dictMutex;

map<string, int> patternDict;
vector<string> filenameDict;
vector<ofImage*> images;


//--------------------------------------------------------------
int filenameToPattern (const string &fn) {
  if (patternDict.count(fn) > 0) return patternDict[fn];
  const int ret = filenameDict.size();
  pthread_mutex_lock(&dictMutex); {
    patternDict[fn] = ret;
    filenameDict.push_back(fn);
  } pthread_mutex_unlock(&dictMutex);
  return ret;
  /*
  cerr << "will load : " << fn << endl;
  const int ret = images.size();
  patternTable[fn] = ret;
  //  pthread_mutex_lock(&imagesMutex); {
  cerr << "new" << ret << endl;
  ofImage *newImg = new ofImage();
  //images.push_back();
  cerr << "lpush" << endl;
  //} pthread_mutex_unlock(&imagesMutex);
  cerr << "load" << endl;
  newImg->loadImage(fn);
  //images[ret]->loadImage(fn);
  cerr << "load : " << fn << endl;
  return ret;*/
  return 0;
}

//========================================================================
// communication 
void* inputter (void* args) {
  int worldIndex = 0;
  const float scale = ofGetHeight();

  // debug input
  //ifstream cin("/Users/nushio/ICFP2011/visualizer/of/bin/test.in");
  
  for (;;) {
    string linestr, cmd;
    getline(cin, linestr);
    istringstream iss(linestr);
    iss >> cmd;
    if (cmd == "flush") {
      pthread_mutex_lock(&worldMutex); {
        swap(fgWorld, bgWorld);
      } pthread_mutex_unlock(&worldMutex);
      
      bgWorld->clear();
      bgWorld->generation = ++worldIndex;
    } else if (cmd == "label") {
      float x,y,fontsize; string text;
      iss >> x >> y >> fontsize;
      getline(iss, text);
      bgWorld->stamps.push_back
        (nfStamp(x*scale,y*scale,0,-1,fontsize,text));
    } else if (cmd == "stamp") {
      float x,y,zoom; string filename;
      iss >> x >> y >> zoom >> filename;
      int pattern = filenameToPattern(filename);
      bgWorld->stamps.push_back
        (nfStamp(x*scale,y*scale,zoom,pattern));
    } else if (cmd == "draw") {
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
  // now load another font, but with extended parameters:
  // font name, size, anti-aliased, full character set
  verdana.loadFont("verdana.ttf",8, false, true);
  verdana.setLineHeight(20.0f);

  fgWorld = new nfWorld();
  bgWorld = new nfWorld();
  drawnWorld = new nfWorld();
  pthread_mutex_init(&worldMutex, NULL);
  pthread_mutex_init(&dictMutex, NULL);

  testImage.loadImage("0-0.png");
}


//--------------------------------------------------------------
void testApp::update(){
  ofBackground(255,255,255);	
}

//--------------------------------------------------------------
void testApp::draw(){
  pthread_mutex_lock(&worldMutex); {
    if (fgWorld->generation > drawnWorld->generation)
      swap(fgWorld, drawnWorld);
  } pthread_mutex_unlock(&worldMutex);

  

  // load images that are new in dictionary
  pthread_mutex_lock(&dictMutex); {
    while (filenameDict.size() > images.size()) {
      int i = images.size();
      images.push_back(new ofImage());
      cerr << "loading " << i << " " << filenameDict[i] << endl;
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
