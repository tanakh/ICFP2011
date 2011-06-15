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

  ofImage testImage;
  ofTrueTypeFont  verdana;
};

struct nfStamp {
  float x, y, zoom;
  int pattern;
  float fontsize;
  string text;
  nfStamp (float x, float y, float zoom, int pattern,
           float fontsize = -1, string text = "") :
    x(x), y(y), zoom(zoom), pattern(pattern),
    fontsize(fontsize), text(text) {}
};

struct nfWorld {
  int generation;
  std::vector<nfStamp> stamps;
  nfWorld () : generation(0) {}
  void clear () {
    stamps.clear();
  }
};

void* inputter (void* args);


#endif	

