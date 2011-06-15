#ifndef _TEST_APP
#define _TEST_APP

#include <string>
#include <vector>

#include "ofMain.h"

class testApp : public ofBaseApp{
	
public:
		
  void setup();
  void update();
  void draw();
		
  void keyPressed(int key);
  void keyReleased(int key);
  void mouseMoved(int x, int y );
  void mouseDragged(int x, int y, int button);
  void mousePressed(int x, int y, int button);
  void mouseReleased(int x, int y, int button);
  void windowResized(int w, int h);
		
  ofTrueTypeFont  verdana;
};

struct nfLabel {
  float x, y, fontsize;
  std::string text;
	
  nfLabel (float x, float y, float fontsize, const std::string &text) :
    x(x), y(y), fontsize(fontsize), text(text) {}
};

struct nfStamp {
  float x, y, zoom;
  int pattern;
  nfStamp (float x, float y, float zoom, int pattern) :
    x(x), y(y), zoom(zoom), pattern(pattern) {}
};

struct nfWorld {
  int generation;
  std::vector<nfLabel> labels;
  std::vector<nfStamp> stamps;
  nfWorld () : generation(0) {}
  void clear () {
    labels.clear(); stamps.clear();
  }
};

void* inputter (void* args);


#endif	

