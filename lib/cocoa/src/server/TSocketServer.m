#import <Cocoa/Cocoa.h>
#import "TSocketServer.h"
#import "TNSFileHandleTransport.h"
#import "TProtocol.h"
#import "TTransportException.h"


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
  mServerSocket = [[NSSocketPort alloc] initWithTCPPort: port];
  // FIXME - move this separate start method and add method to close
  // and cleanup any open ports

  if (mServerSocket == nil) {
    NSLog(@"Unable to listen on TCP port %d", port);
  } else {
    NSLog(@"Listening on TCP port %d", port);

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
  }

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


- (void) connectionAccepted: (NSNotification *) aNotification
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

  @try {
    while ([mProcessor processOnInputProtocol: inProtocol outputProtocol: outProtocol]);
  }
  @catch (TTransportException * te) {
    NSLog(@"%@", te);
  }

  [pool release];
}



@end



