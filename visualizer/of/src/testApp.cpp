#include <map>
#include <pthread.h>

#include "testApp.h"

using namespace std;


nfWorld *fgWorld, *bgWorld, *drawnWorld;
pthread_mutex_t worldMutex;
pthread_mutex_t imagesMutex;

map<string, int> patternTable;
vector<ofImage> images;

//========================================================================
// communication 
void* inputter (void* args) {
  int worldIndex = 0;
  for (;;) {
    string linestr, cmd;
    getline(cin, linestr);
    istringstream iss(linestr);
    iss >> cmd;
    if (cmd == "flush") {
      pthread_mutex_lock(&worldMutex);
      swap(fgWorld, bgWorld);
      pthread_mutex_unlock(&worldMutex);
      bgWorld->clear();
      bgWorld->generation = ++worldIndex;
    } else if (cmd == "label") {
      float x,y,fontsize; string text;
      iss >> x >> y >> fontsize;
      getline(iss, text);
      bgWorld->labels.push_back(nfLabel(x,y,fontsize,text));
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
  //stamps.loadImage("0.png");

  fgWorld = new nfWorld();
  bgWorld = new nfWorld();
  drawnWorld = new nfWorld();
  pthread_mutex_init(&worldMutex, NULL);
  pthread_mutex_init(&imagesMutex, NULL);
}


//--------------------------------------------------------------
void testApp::update(){
  ofBackground(255,255,255);	
}

//--------------------------------------------------------------
void testApp::draw(){
  pthread_mutex_lock(&worldMutex);
  if (fgWorld->generation > drawnWorld->generation)
    swap(fgWorld, drawnWorld);
  pthread_mutex_unlock(&worldMutex);

  const float screenHeight = ofGetHeight();
  
  const vector<nfLabel> &labels = drawnWorld->labels;
  for (int i = 0; i < labels.size(); ++i) {
    ofPushMatrix(); {
      const nfLabel &label = labels[i];
      ofTranslate(label.x * screenHeight, label.y * screenHeight, 0);
      ofSetColor(0x000000);
      ofScale(label.fontsize, label.fontsize, 1);
      verdana.drawString(label.text,0,0);
    }
    ofPopMatrix();
  }
  /*
    ofSetColor(0xFFFFFF);
    ofEnableAlphaBlending();
    stamps.draw(0,0);
    ofDisableAlphaBlending();
  */
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
