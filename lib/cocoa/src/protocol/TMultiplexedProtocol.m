//
//  TMultiplexedProtocol.m
//  TaLiCaiCommunity
//
//  Created by guoyalun on 7/3/14.
//  Copyright (c) 2014 guoyalun. All rights reserved.
//

#import "TMultiplexedProtocol.h"

static NSString *SEPARATOR = @":";

@implementation TMultiplexedProtocol

- (id)initWithProtocol:(id <TProtocol>)protocol serviceName:(NSString *)serviceName
{
    if (self = [super initWithProtocol:protocol]) {
        _serviceName = serviceName;
    }
    return self;
}

- (void) writeMessageBeginWithName: (NSString *) name
                              type: (int) messageType
                        sequenceID: (int) sequenceID
{
    if (messageType == TMessageType_CALL || messageType == TMessageType_ONEWAY) {
        
        NSString *messageName = [NSString stringWithFormat:@"%@%@%@",_serviceName,SEPARATOR,name];
        [super writeMessageBeginWithName:messageName type:messageType sequenceID:sequenceID];
    } else {
        [super writeMessageBeginWithName:name type:messageType sequenceID:sequenceID];
    }
}

@end
