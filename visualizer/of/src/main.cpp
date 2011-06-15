#include <iostream>
#include <string>
#include <pthread.h>

#include "ofMain.h"
#include "testApp.h"
#include "ofAppGlutWindow.h"

using namespace std;
string globalMessage;

void* inputter (void* args) {
	string s;
	for (;;) {
		getline(cin, s);
		globalMessage = s;
	}
}

//========================================================================
int main( ){

    ofAppGlutWindow window;
	ofSetupOpenGL(&window, 800,700, OF_WINDOW);			// <-------- setup the GL context
    globalMessage = "";

	// create a thread that overwrites the global message.
	pthread_t thread;
	pthread_create(&thread, NULL, inputter ,(void*) NULL);

	// this kicks off the running of my app
	// can be OF_WINDOW or OF_FULLSCREEN
	// pass in width and height too:
	ofRunApp( new testApp());
	pthread_detach(thread);
}
