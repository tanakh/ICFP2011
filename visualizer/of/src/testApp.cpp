#include "testApp.h"


//--------------------------------------------------------------
void testApp::setup(){	 
	// now load another font, but with extended parameters:
	// font name, size, anti-aliased, full character set
	verdana.loadFont("verdana.ttf",8, false, true);
	verdana.setLineHeight(20.0f);
	stamps.loadImage("0.png");
}


//--------------------------------------------------------------
void testApp::update(){
	ofBackground(255,0,128);	
}

//--------------------------------------------------------------
void testApp::draw(){

	

	ofPushMatrix();
		ofTranslate(100,100,0);
		ofSetColor(0x000000);
		verdana.drawString(globalMessage,0,0);
	ofPopMatrix();

	ofSetColor(0xFFFFFF);
	ofEnableAlphaBlending();
	stamps.draw(0,0);
	ofDisableAlphaBlending();

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
