//
//  TMultiplexedProtocol.h
//  TaLiCaiCommunity
//
//  Created by guoyalun on 7/3/14.
//  Copyright (c) 2014 guoyalun. All rights reserved.
//

#import "TProtocolDecorator.h"

@interface TMultiplexedProtocol : TProtocolDecorator
{
    NSString *_serviceName;
}

- (id)initWithProtocol:(id <TProtocol>)protocol serviceName:(NSString *)serviceName;


@end
