#import <Cocoa/Cocoa.h>
#import "TSocketServer.h"
#import "TNSFileHandleTransport.h"
#import "TProtocol.h"


@implementation TSocketServer

- (id) initWithPort: (int) port
    protocolFactory: (id <TProtocolFactory>) protocolFactory
          processor: (id <TProcessor>) processor;
{
  self = [super init];

  mInputProtocolFactory = [protocolFactory retain];
  mOutputProtocolFactory = [protocolFactory retain];
  mProcessor = [processor retain];
  
  // create a socket
  mServerSocket = [[NSSocketPort alloc] initWithTCPPort: 8081];
  // wrap it in a file handle so we can get messages from it
  mSocketFileHandle = [[NSFileHandle alloc] initWithFileDescriptor: [mServerSocket socket]
                                            closeOnDealloc: YES];

  // register for notifications of accepted incoming connections
  [[NSNotificationCenter defaultCenter] addObserver: self 
                                        selector: @selector(connectionAccepted:) 
                                        name: NSFileHandleConnectionAcceptedNotification
                                        object: mSocketFileHandle];

  // tell socket to listen
  [mSocketFileHandle acceptConnectionInBackgroundAndNotify];
  
  return self;
}


- (void) dealloc {
  [mInputProtocolFactory release];
  [mOutputProtocolFactory release];
  [mProcessor release];
  [mSocketFileHandle release];
  [mServerSocket release];
  [super dealloc];
}


- (void) connentionAccepted: (NSNotification *) aNotification
{
  NSFileHandle * socket = [[aNotification userInfo] objectForKey: NSFileHandleNotificationFileHandleItem];
  
  // now that we have a client connected, spin off a thread to handle activity
  [NSThread detachNewThreadSelector: @selector(handleClientConnection:)
                           toTarget: self
                         withObject: socket];
  
  [[aNotification object] acceptConnectionInBackgroundAndNotify];
}


- (void) handleClientConnection: (NSFileHandle *) clientSocket
{
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

  TNSFileHandleTransport * transport = [[TNSFileHandleTransport alloc] initWithFileHandle: clientSocket];

  id <TProtocol> inProtocol = [mInputProtocolFactory newProtocolOnTransport: transport];
  id <TProtocol> outProtocol = [mOutputProtocolFactory newProtocolOnTransport: transport];
  
  while ([mProcessor processOnInputProtocol: inProtocol outputProtocol: outProtocol]);
  
  [pool release];
}



@end



